package andreasWallner.blackbox.ice40

import spinal.core._
import spinal.lib.blackbox.lattice.ice40.SB_PLL40_PAD_CONFIG
import andreasWallner.ext._

import scala.language.postfixOps

object SB_PLL40_PAD_CONFIG_EXT {
  case class Dividers(
                       divr: Int,
                       divf: Int,
                       divq: Int,
                       simple_fb: Boolean,
                       fin: HertzNumber,
                       fdiv: HertzNumber,
                       fvco: HertzNumber,
                       fout: HertzNumber
                     ) {}

  def calculate(
                 fin: HertzNumber,
                 fout_req: HertzNumber,
                 simple_fb_req: Option[Boolean],
                 allowed_mismatch_percent: Double
               ): Dividers = {
    import util.control.Breaks._

    // see iCE40 LP/HP Family Data Sheet, FPGA-DS-02029-4.0, p. 35
    if (fin < (10.0 MHz) || fin > (133.0 MHz))
      throw new Exception(s"PLL input must be in range [10.0, 133.0], not $fin")
    // see iCE40 LP/HP Family Data Sheet, FPGA-DS-02029-4.0, p. 35
    if (fout_req < (16.0 MHz) || fout_req > (275.0 MHz)) {
      val (fout_eng, fout_postfix) = fout_req.decompose
      throw new Exception(
        s"PLL output must be in range [16.0, 275.0], not $fout_eng $fout_postfix"
      )
    }

    def better_match(a: Option[Dividers], b: Dividers): Option[Dividers] = {
      (a, b) match {
        case (None, _) => Some(b)
        case (Some(aa), bb) if (aa.fout - fout_req).abs < (b.fout - fout_req).abs => a
        case _ => Some(b)
      }
    }

    var best: Option[Dividers] = None
    for (simple_fb <- Seq(true, false)
         if (simple_fb_req.isEmpty || simple_fb == simple_fb_req.get)) {
      val divf_max = if (simple_fb) 127 else 63
      for (div <- 0 to 15) {
        breakable {
          val fdiv = fin / (div + 1.0)
          if (fdiv < (10.0 MHz) || fdiv > (133.0 MHz))
            break

          for (divf <- 0 to divf_max) {
            breakable {
              if (simple_fb) {
                val fvco = fdiv * (divf + 1.0)
                if (fvco < (533.0 MHz) || fvco > (1066.0 MHz))
                  break

                for (divq <- 1 to 6) {
                  val fout = fvco / (1 << (divq - 1))
                  best = better_match(
                    best,
                    Dividers(div, divf, divq, simple_fb, fin, fdiv, fvco, fout)
                  )
                }
              } else {
                for (divq <- 1 to 6) {
                  val fvco = fdiv * (divf + 1.0) * (1 << (divq - 1))
                  if (fvco < 533.0 || fvco > 1066.0)
                    break
                  val fout = fvco / (1 << (divq - 1))
                  best = better_match(
                    best,
                    Dividers(div, divf, divq, simple_fb, fin, fdiv, fvco, fout)
                  )
                }
              }
            }
          }
        }
      }
    }

    if (best.isEmpty)
      throw new Exception(s"Count not find PLL configuration for fin=${fin} fout=${fout_req}")
    val solution = best.get
    if (((solution.fout - fout_req).abs / fout_req) > allowed_mismatch_percent)
      throw new Exception(
        s"Could not find PLL configuration for fin=${fin} fout=${fout_req} within ${allowed_mismatch_percent}% -- best match is ${best}"
      )
    solution
  }

  def make_config(
                   fin: HertzNumber,
                   fout: HertzNumber,
                   allowed_mismatch_percent: Double = 1.0,
                   FEEDBACK_PATH: Option[String] = None, // DELAY, SIMPLE, PHASE_AND_DELAY or EXTERNAL
                   DELAY_ADJUSTMENT_MODE_FEEDBACK: String = "FIXED", // FIXED or DYNAMIC
                   FDA_FEEDBACK: Bits = B(0, 4 bit),
                   DELAY_ADJUSTMENT_MODE_RELATIVE: String = "FIXED", // FIXED or DYNAMIC
                   FDA_RELATIVE: Bits = B(0, 4 bit),
                   SHIFTREG_DIV_MODE: Bits = B(0, 2 bit),
                   PLLOUT_SELECT: String = "GENCLK", // GENCLK, GENCLK_HALF, SHIFTREG_90deg, SHIFTREG_0deg
                   ENABLE_ICEGATE: Bool = False
                 ): SB_PLL40_PAD_CONFIG = {
    val best = calculate(
      fin,
      fout,
      FEEDBACK_PATH.map(x => x == "SIMPLE"),
      allowed_mismatch_percent
    )
    val filter_range = best.fdiv match {
      case x if x < 17 => 1
      case x if x < 26 => 2
      case x if x < 44 => 3
      case x if x < 66 => 4
      case x if x < 101 => 5
      case _ => 6
    }

    print(best)

    SB_PLL40_PAD_CONFIG(
      B(best.divr, 4 bit),
      B(best.divf, 7 bit),
      B(best.divq, 3 bit),
      B(filter_range, 3 bit),
      FEEDBACK_PATH.getOrElse(if (best.simple_fb) "SIMPLE" else "DELAY"),
      DELAY_ADJUSTMENT_MODE_FEEDBACK,
      FDA_FEEDBACK,
      DELAY_ADJUSTMENT_MODE_RELATIVE,
      FDA_RELATIVE,
      SHIFTREG_DIV_MODE,
      PLLOUT_SELECT,
      ENABLE_ICEGATE //,
      //None // EXTERNAL_DIVIDE_FACTOR missing
    )
  }
}

case class SB_PLL40_CORE(p: SB_PLL40_PAD_CONFIG) extends BlackBox {
  val REFERENCECLK = in Bool()
  val PLLOUTCORE = out Bool()
  val PLLOUTGLOBAL = out Bool()
  val RESETB = in Bool()
  val BYPASS = in Bool()
  //val LOCK = out Bool()
  //val LATCHINPUTVALUE = in Bool()

  p.applyTo(this)
}
