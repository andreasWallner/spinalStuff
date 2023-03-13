package andreasWallner.blackbox.ice40

import andreasWallner.Utils.time
import spinal.core._
import spinal.lib.blackbox.lattice.ice40.SB_PLL40_PAD_CONFIG

import scala.language.postfixOps

sealed trait FeedbackPath {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

//noinspection ScalaUnusedSymbol
object FeedbackPath {
  case object SIMPLE extends FeedbackPath
  case object DELAY extends FeedbackPath
  case object PHASE_AND_DELAY extends FeedbackPath
  case object EXTERNAL extends FeedbackPath
}

sealed trait PllOutSelect {
  override def toString: String = this.getClass.getSimpleName.dropRight(1)
}

//noinspection ScalaUnusedSymbol
case object PllOutSelect {
  case object GENCLK extends PllOutSelect
  case object GENCLK_HALF extends PllOutSelect
  case object SHIFTREG_90deg extends PllOutSelect
  case object SHIFTREG_0deg extends PllOutSelect
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
      simple_fb: Boolean
  ) {

    def fdiv = fin / (divr + 1)
    def fvco = if (simple_fb) fdiv * (divf + 1) else fdiv * (divf + 1) * (1 << divq)
    def fout = fvco / (1 << divq)

    def validFreqs =
      fdiv >= fdiv_min && fdiv <= fdiv_max && fvco >= fvco_min && fvco <= fvco_max && fout >= fout_min && fout <= fout_max

    def report(): String = {
      val feedback = if (simple_fb) "SIMPLE" else "NON SIMPLE"
      f"""
        |input freq:   ${fin.decompose._1} ${fin.decompose._2}
        |output freq:  ${fout.decompose._1} ${fout.decompose._2}
        |
        |feedback: $feedback
        |PFD freq: ${fdiv.decompose._1} ${fdiv.decompose._2}
        |VCO freq: ${fvco.decompose._1} ${fdiv.decompose._2}
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
      simple_fb_req: Option[Boolean],
      allowed_mismatch_percent: Double
  ): Settings = {
    require(fin >= fin_min && fin <= fin_max, s"PLL input must be in range [10.0, 133.0], not $fin")
    require(
      fout_req >= fout_min && fout_req <= fout_max,
      s"PLL output must be in range [16.0, 275.0], not ${fout_req.decompose}"
    )

    def better_match(a: Option[Settings], b: Option[Settings]): Option[Settings] = {
      (a, b) match {
        case (None, Some(_))                                                             => b
        case (Some(_), None)                                                             => a
        case (Some(aa), Some(bb)) if (aa.fout - fout_req).abs < (bb.fout - fout_req).abs => a
        case _                                                                           => b
      }
    }

    def best_div(simple_fb: Boolean): Option[Settings] = {
      var best: Option[Settings] = None

      def update_if_valid(divr: Int, divf: Int, divq: Int): Unit = {
        // icepll mentions that the PLL Usage guide lists the wrong limit for DIVF for simple feedback
        // https://github.com/YosysHQ/icestorm/blob/d20a5e9001f46262bf0cef220f1a6943946e421d/icepll/icepll.cc#L93
        if (divf < 0 || divf > 127 || (!simple_fb && divf > 63))
          return

        val settings = Settings(fin, divr, divf, divq, simple_fb = simple_fb)
        if (!settings.validFreqs)
          return

        best = better_match(best, Some(settings))
      }

      if (simple_fb) {
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

    val best =
      better_match(
        if (simple_fb_req.isEmpty || simple_fb_req.get) best_div(true) else None,
        if (simple_fb_req.isEmpty || !simple_fb_req.get) best_div(false) else None
      )

    if (best.isEmpty)
      throw new Exception(s"Could not find any PLL configuration for fin=$fin fout=$fout_req")
    val solution = best.get
    if (((solution.fout - fout_req).abs / fout_req) > allowed_mismatch_percent)
      throw new Exception(
        s"Could not find PLL configuration for fin=$fin fout=$fout_req within $allowed_mismatch_percent% -- best match is $best"
      )
    solution
  }

  def make_config(
      fin: HertzNumber,
      fout: HertzNumber,
      allowed_mismatch_percent: Double = 1.0,
      FEEDBACK_PATH: Option[FeedbackPath] = Some(FeedbackPath.SIMPLE),
      FDA_FEEDBACK: Option[Bits] = None,
      FDA_RELATIVE: Option[Bits] = None,
      SHIFTREG_DIV_MODE: Bits = B(0, 2 bit),
      PLLOUT_SELECT: PllOutSelect = PllOutSelect.GENCLK,
      ENABLE_ICEGATE: Bool = False
  ): SB_PLL40_PAD_CONFIG = {
    val best = calculate(
      fin,
      fout,
      FEEDBACK_PATH.map(x => x == FeedbackPath.SIMPLE),
      allowed_mismatch_percent
    )
    val filter_range = best.fdiv match {
      case x if x < (17 MHz)  => 1
      case x if x < (26 MHz)  => 2
      case x if x < (44 MHz)  => 3
      case x if x < (66 MHz)  => 4
      case x if x < (101 MHz) => 5
      case _                  => 6
    }

    SB_PLL40_PAD_CONFIG(
      B(best.divr, 4 bit),
      B(best.divf, 7 bit),
      B(best.divq, 3 bit),
      B(filter_range, 3 bit),
      FEEDBACK_PATH.map(x => x.toString).getOrElse(if (best.simple_fb) "SIMPLE" else "DELAY"),
      if (FDA_FEEDBACK.isDefined) "DYNAMIC" else "FIXED",
      FDA_FEEDBACK.getOrElse(B(0, 4 bit)),
      if (FDA_RELATIVE.isDefined) "DYNAMIC" else "FIXED",
      FDA_RELATIVE.getOrElse(B(0, 4 bit)),
      SHIFTREG_DIV_MODE,
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

/*
object Benchmark extends App {
  (0 to 2) foreach { i =>
    val f = time("false") {
      SB_PLL40_PAD_CONFIG_EXT.calculate(33 MHz, 200 MHz, Some(true), 1.0, false)
    }
    val t = time("true ") {
      SB_PLL40_PAD_CONFIG_EXT.calculate(33 MHz, 200 MHz, Some(true), 1.0, true)
    }
    println()
    if (i == 0) {
      println(f.report())
      println(t.report())
    }
  }
}
*/
