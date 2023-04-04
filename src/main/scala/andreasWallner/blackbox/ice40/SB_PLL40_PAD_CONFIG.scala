package andreasWallner.blackbox.ice40

import andreasWallner.Utils.time
import spinal.core._
import spinal.lib.blackbox.lattice.ice40.SB_PLL40_PAD_CONFIG

import scala.language.postfixOps

sealed trait FeedbackPath {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

object FeedbackPath {
  case object SIMPLE extends FeedbackPath
  case object DELAY extends FeedbackPath
  case object PHASE_AND_DELAY extends FeedbackPath
  case object EXTERNAL extends FeedbackPath
}

sealed trait PllOutSelect {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

case object PllOutSelect {
  case object GENCLK extends PllOutSelect
  case object GENCLK_HALF extends PllOutSelect
  case object SHIFTREG_90deg extends PllOutSelect
  case object SHIFTREG_0deg extends PllOutSelect
}

sealed trait ShiftregDivMode;
case object ShiftregDivMode {
  case object DIV_4 extends ShiftregDivMode;
  case object DIV_7 extends ShiftregDivMode;
}

object SB_PLL40_PAD_CONFIG_EXT {
  // see iCE40 LP/HP Family Data Sheet, FPGA-DS-02029-4.0, p. 35
  val fin_min = 10 MHz
  val fin_max = 133 MHz
  val fvco_min = 533 MHz
  val fvco_max = 1066 MHz
  val fout_min = 16 MHz
  val fout_max = 275 MHz
  // not from spec, taken from icepll
  // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#LL110C43-L110C43
  val fdiv_min = 10 MHz
  val fdiv_max = 133 MHz

  case class Settings(
      fin: HertzNumber,
      divr: Int,
      divf: Int,
      divq: Int,
      feedback: FeedbackPath,
      private val shiftreg_div: Int = 1
  ) {
    def fdiv = fin / (divr + 1)
    // note that PLL uses 4x frequency internally in PHASE_AND_DELAY mode
    def fvco = feedback match {
      case FeedbackPath.SIMPLE                        => fdiv * (divf + 1)
      case FeedbackPath.DELAY | FeedbackPath.EXTERNAL => fdiv * (divf + 1) * (1 << divq)
      case FeedbackPath.PHASE_AND_DELAY               => fdiv * (divf + 1) * (1 << divq) * shiftreg_div
    }
    def fout = feedback match {
      case FeedbackPath.PHASE_AND_DELAY => fvco / (1 << divq) / shiftreg_div
      case _                            => fvco / (1 << divq)
    }

    def isValid =
      fdiv >= fdiv_min && fdiv <= fdiv_max && fvco >= fvco_min && fvco <= fvco_max && fout >= fout_min && fout <= fout_max

    def report(): String = {
      val feedbackStr =
        if (feedback == FeedbackPath.SIMPLE) "SIMPLE" else "DELAY or PHASE_AND_DELAY"
      f"""
        |input freq:   $fin
        |output freq:  $fout
        |
        |feedback: $feedbackStr
        |PFD freq: $fdiv
        |VCO freq: $fvco
        |
        |DIVR: $divr
        |DIVF: $divf
        |DIVQ: $divq
        |""".stripMargin
    }
  }

  def calculate(
      fin: HertzNumber,
      fout_req: HertzNumber,
      feedback_req: Option[FeedbackPath],
      shiftreg_div: Int = 1
  ): Option[Settings] = {
    require(fin >= fin_min && fin <= fin_max, s"PLL input must be in range [10.0, 133.0], not $fin")
    require(
      fout_req >= fout_min && fout_req <= fout_max,
      s"PLL output must be in range [16.0, 275.0], not $fout_req"
    )

    def better_match(a: Option[Settings], b: Option[Settings]): Option[Settings] = {
      (a, b) match {
        case (None, Some(_))                                                             => b
        case (Some(_), None)                                                             => a
        case (Some(aa), Some(bb)) if (aa.fout - fout_req).abs < (bb.fout - fout_req).abs => a
        case _                                                                           => b
      }
    }

    def best_div(feedback: FeedbackPath): Option[Settings] = {
      var best: Option[Settings] = None

      def update_if_valid(divr: Int, divf: Int, divq: Int): Unit = {
        // icepll mentions that the PLL Usage guide lists the wrong limit for DIVF for simple feedback
        // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#L93
        if (divf < 0 || divf > 127 || (feedback != FeedbackPath.SIMPLE && divf > 63))
          return

        val settings = Settings(fin, divr, divf, divq, feedback, shiftreg_div)
        if (!settings.isValid)
          return

        best = better_match(best, Some(settings))
      }

      if (feedback == FeedbackPath.SIMPLE) {
        for (divr <- 0 until 16) {
          for (divq <- 0 until 8) {
            val divf_exact = ((fout_req * (divr + 1) * (1 << divq)) / fin) - 1
            update_if_valid(divr, divf_exact.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt, divq)
            update_if_valid(
              divr,
              divf_exact.setScale(0, BigDecimal.RoundingMode.CEILING).toInt,
              divq
            )
          }
        }
      } else {
        for (divr <- 0 until 16) {
          val divf_exact = ((fout_req * (divr + 1)) / fin) - 1
          for (divq <- 0 until 8) {
            update_if_valid(divr, divf_exact.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt, divq)
            update_if_valid(
              divr,
              divf_exact.setScale(0, BigDecimal.RoundingMode.CEILING).toInt,
              divq
            )
          }
        }
      }
      best
    }

    feedback_req match {
      case Some(x) => best_div(x)
      case None    => better_match(best_div(FeedbackPath.SIMPLE), best_div(FeedbackPath.DELAY))
    }
  }

  def singleOutput(
      fin: HertzNumber,
      fout: HertzNumber,
      allowed_mismatch: Double = 0.01,
      FEEDBACK_PATH: Option[FeedbackPath] = Some(FeedbackPath.SIMPLE),
      FDA_FEEDBACK: Option[Bits] = None,
      FDA_RELATIVE: Option[Bits] = None,
      SHIFTREG_DIV_MODE: Option[ShiftregDivMode] = None,
      PLLOUT_SELECT: PllOutSelect = PllOutSelect.GENCLK,
      ENABLE_ICEGATE: Bool = False
  ): SB_PLL40_PAD_CONFIG = {
    require(
      FEEDBACK_PATH.isEmpty || FEEDBACK_PATH.get != FeedbackPath.PHASE_AND_DELAY || PLLOUT_SELECT == PllOutSelect.SHIFTREG_0deg || PLLOUT_SELECT == PllOutSelect.SHIFTREG_90deg,
      "if feedback path is PHASE_AND_DELAY, output select must be SHIFTREG_Xdeg"
    )
    require(
      SHIFTREG_DIV_MODE.isEmpty || FEEDBACK_PATH
        .getOrElse(FeedbackPath.SIMPLE) == FeedbackPath.PHASE_AND_DELAY,
      "SHIFTREG_DIV_MODE can only be used in PHASE_AND_DELAY feedback mode"
    )
    require(
      (PLLOUT_SELECT != PllOutSelect.SHIFTREG_0deg && PLLOUT_SELECT != PllOutSelect.SHIFTREG_90deg) || (FEEDBACK_PATH.isDefined && FEEDBACK_PATH.get == FeedbackPath.PHASE_AND_DELAY),
      "SHIFTREG_Xdeg output selection can only be used with PHASE_AND_DELAY feedback mode"
    )

    val best = calculate(
      fin,
      fout,
      FEEDBACK_PATH,
      SHIFTREG_DIV_MODE match {
        case Some(ShiftregDivMode.DIV_4) => 4
        case Some(ShiftregDivMode.DIV_7) => 7
        case None => 4 // ignored if feedback mode is != PHASE_AND_DELAY, and we default to 4 if not set
      }
    )
    if (best.isEmpty)
      throw new Exception(s"Could not find any PLL configuration for fin=$fin fout=$fout")

    println(best.get.report())
    val solution = best.get
    if (((solution.fout - fout).abs / fout) > allowed_mismatch)
      throw new Exception(
        s"Could not find PLL configuration for fin=$fin fout=$fout within ${allowed_mismatch * 100}%\n" +
          s"  best match is ${((solution.fout - fout).abs / fout) * 100}% off:${best.get.report()}"
      )

    // not from spec, values taken from icepll
    // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#L316
    val filter_range = solution.fdiv match {
      case x if x < (17 MHz)  => 1
      case x if x < (26 MHz)  => 2
      case x if x < (44 MHz)  => 3
      case x if x < (66 MHz)  => 4
      case x if x < (101 MHz) => 5
      case _                  => 6
    }

    SB_PLL40_PAD_CONFIG(
      B(solution.divr, 4 bit),
      B(solution.divf, 7 bit),
      B(solution.divq, 3 bit),
      B(filter_range, 3 bit),
      FEEDBACK_PATH.map(x => x.toString).getOrElse(solution.feedback.toString),
      if (FDA_FEEDBACK.isDefined) "DYNAMIC" else "FIXED",
      FDA_FEEDBACK.getOrElse(B(0, 4 bit)),
      if (FDA_RELATIVE.isDefined) "DYNAMIC" else "FIXED",
      FDA_RELATIVE.getOrElse(B(0, 4 bit)),
      SHIFTREG_DIV_MODE match {
        case Some(ShiftregDivMode.DIV_4) => B(0, 1 bit)
        case Some(ShiftregDivMode.DIV_7) => B(1, 1 bit)
        case None                        => B(0, 1 bit)
      },
      PLLOUT_SELECT.toString,
      ENABLE_ICEGATE //,
      //None // EXTERNAL_DIVIDE_FACTOR missing
    )
  }
}

//noinspection ScalaUnusedSymbol
abstract class ICE40_PLL(p: SB_PLL40_PAD_CONFIG, withLock: Boolean = false) extends BlackBox {
  val RESETB = in Bool ()
  val BYPASS = in Bool ()
  val EXTFEEDBACK = if (p.FEEDBACK_PATH == "EXTERNAL") in Bool () else null
  val DYNAMICDELAY =
    if (p.DELAY_ADJUSTMENT_MODE_FEEDBACK == "DYNAMIC" || p.DELAY_ADJUSTMENT_MODE_RELATIVE == "DYNAMIC")
      in Bits (8 bit)
    else null
  val LATCHINPUTVALUE = if (p.ENABLE_ICEGATE == True) in Bool () else null
  val LOCK = if (withLock) out Bool () else null
  val PLLOUTGLOBAL = out Bool ()
  val PLLOUTCORE = out Bool ()

  def clockInput: Bool
}

case class SB_PLL40_CORE(p: SB_PLL40_PAD_CONFIG, withLock: Boolean = false)
    extends ICE40_PLL(p, withLock) {
  val REFERENCECLK = in Bool ()

  p.applyTo(this)

  override def clockInput = REFERENCECLK
}

case class SB_PLL40_PAD(p: SB_PLL40_PAD_CONFIG, withLock: Boolean = false)
    extends ICE40_PLL(p, withLock) {
  val PACKAGEPIN = in Bool ()

  p.applyTo(this)

  override def clockInput = PACKAGEPIN
}

