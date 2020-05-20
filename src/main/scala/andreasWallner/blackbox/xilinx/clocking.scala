package andreasWallner.blackbox.xilinx

import spinal.core._

object IBUFG {
  def on(that: Bool): Bool = {
    val ibufg = IBUFG()
    ibufg.setCompositeName(that, "IBUFG")
    ibufg.I := that
    ibufg.O
  }
}

case class IBUFG() extends BlackBox {
  val I = in Bool()
  val O = out Bool()
}

// See UG472 (v1.14), 7 Series Clocking, p. 78 ff
case class MMCME2_BASE(
    CLKIN1_PERIOD: Double,
    BANDWIDTH: String = "OPTIMIZED",
    CLKFBOUT_MULT_F: Double = 5.0,
    CLKFBOUT_PHASE: Double = 0.0,
    CLKOUT0_DIVIDE_F: Double = 1.0,
    CLKOUT1_DIVIDE: Double = 1,
    CLKOUT2_DIVIDE: Double = 1,
    CLKOUT3_DIVIDE: Double = 1,
    CLKOUT4_DIVIDE: Double = 1,
    CLKOUT5_DIVIDE: Double = 1,
    CLKOUT0_DUTY_CYCLE: Double = 0.5,
    CLKOUT1_DUTY_CYCLE: Double = 0.5,
    CLKOUT2_DUTY_CYCLE: Double = 0.5,
    CLKOUT3_DUTY_CYCLE: Double = 0.5,
    CLKOUT4_DUTY_CYCLE: Double = 0.5,
    CLKOUT5_DUTY_CYCLE: Double = 0.5,
    CLKOUT0_PHASE: Double = 0.0,
    CLKOUT1_PHASE: Double = 0.0,
    CLKOUT2_PHASE: Double = 0.0,
    CLKOUT3_PHASE: Double = 0.0,
    CLKOUT4_PHASE: Double = 0.0,
    CLKOUT5_PHASE: Double = 0.0,
    CLKOUT4_CASCADE: String = "FALSE",
    DIVCLK_DIVIDE: Double = 1.0,
    REF_JITTER1: Double = 0.0,
    STARTUP_WAIT: String = "FALSE"
) extends BlackBox {
  assert(List("OPTIMIZED", "HIGH", "LOW").contains(BANDWIDTH), message = "Invalid BANDWIDTH")
  assert(CLKFBOUT_MULT_F >= 5.0 && CLKFBOUT_MULT_F <= 64.0, message = "Invalid CLKFBOUT_MULT_F")
  assert(CLKFBOUT_PHASE >= -360.0 && CLKFBOUT_PHASE <= 360.0)
  assert(CLKIN1_PERIOD >= 1.0 && CLKIN1_PERIOD <= 1000.0)
  assert(CLKOUT0_DIVIDE_F >= 1.0 && CLKOUT0_DIVIDE_F <= 128.0)
  assert(CLKOUT0_DUTY_CYCLE >= 0.001 && CLKOUT0_DUTY_CYCLE <= 0.999)
  assert(CLKOUT1_DUTY_CYCLE >= 0.001 && CLKOUT1_DUTY_CYCLE <= 0.999)
  assert(CLKOUT2_DUTY_CYCLE >= 0.001 && CLKOUT2_DUTY_CYCLE <= 0.999)
  assert(CLKOUT3_DUTY_CYCLE >= 0.001 && CLKOUT3_DUTY_CYCLE <= 0.999)
  assert(CLKOUT4_DUTY_CYCLE >= 0.001 && CLKOUT4_DUTY_CYCLE <= 0.999)
  assert(CLKOUT5_DUTY_CYCLE >= 0.001 && CLKOUT5_DUTY_CYCLE <= 0.999)
  assert(CLKOUT0_PHASE >= -360.0 && CLKOUT0_PHASE <= 360.0)
  assert(CLKOUT1_PHASE >= -360.0 && CLKOUT1_PHASE <= 360.0)
  assert(CLKOUT2_PHASE >= -360.0 && CLKOUT2_PHASE <= 360.0)
  assert(CLKOUT3_PHASE >= -360.0 && CLKOUT3_PHASE <= 360.0)
  assert(CLKOUT4_PHASE >= -360.0 && CLKOUT4_PHASE <= 360.0)
  assert(CLKOUT5_PHASE >= -360.0 && CLKOUT5_PHASE <= 360.0)
  assert(DIVCLK_DIVIDE >= 1 && DIVCLK_DIVIDE <= 128)
  assert(REF_JITTER1 >= 0.0 && REF_JITTER1 <= 0.999)

  addGeneric("BANDWIDTH", BANDWIDTH)
  addGeneric("CLKFBOUT_MULT_F", CLKFBOUT_MULT_F)
  addGeneric("CLKFBOUT_PHASE", CLKFBOUT_PHASE)
  addGeneric("CLKIN1_PERIOD", CLKIN1_PERIOD)
  addGeneric("CLKOUT0_DIVIDE_F", CLKOUT0_DIVIDE_F)
  addGeneric("CLKOUT1_DIVIDE", CLKOUT1_DIVIDE)
  addGeneric("CLKOUT2_DIVIDE", CLKOUT2_DIVIDE)
  addGeneric("CLKOUT3_DIVIDE", CLKOUT3_DIVIDE)
  addGeneric("CLKOUT4_DIVIDE", CLKOUT4_DIVIDE)
  addGeneric("CLKOUT5_DIVIDE", CLKOUT5_DIVIDE)
  addGeneric("CLKOUT0_DUTY_CYCLE", CLKOUT0_DUTY_CYCLE)
  addGeneric("CLKOUT1_DUTY_CYCLE", CLKOUT1_DUTY_CYCLE)
  addGeneric("CLKOUT2_DUTY_CYCLE", CLKOUT2_DUTY_CYCLE)
  addGeneric("CLKOUT3_DUTY_CYCLE", CLKOUT3_DUTY_CYCLE)
  addGeneric("CLKOUT4_DUTY_CYCLE", CLKOUT4_DUTY_CYCLE)
  addGeneric("CLKOUT5_DUTY_CYCLE", CLKOUT5_DUTY_CYCLE)
  addGeneric("CLKOUT0_PHASE", CLKOUT0_PHASE)
  addGeneric("CLKOUT1_PHASE", CLKOUT1_PHASE)
  addGeneric("CLKOUT2_PHASE", CLKOUT2_PHASE)
  addGeneric("CLKOUT3_PHASE", CLKOUT3_PHASE)
  addGeneric("CLKOUT4_PHASE", CLKOUT4_PHASE)
  addGeneric("CLKOUT5_PHASE", CLKOUT5_PHASE)
  addGeneric("CLKOUT4_CASCADE", CLKOUT4_CASCADE)
  addGeneric("DIVCLK_DIVIDE", DIVCLK_DIVIDE)
  addGeneric("REF_JITTER1", REF_JITTER1)
  addGeneric("STARTUP_WAIT", STARTUP_WAIT)

  val CLKIN1 = in Bool ()
  val CLKOUT0 = out Bool ()
  val CLKOUT0B = out Bool ()
  val CLKOUT1 = out Bool ()
  val CLKOUT1B = out Bool ()
  val CLKOUT2 = out Bool ()
  val CLKOUT2B = out Bool ()
  val CLKOUT3 = out Bool ()
  val CLKOUT3B = out Bool ()
  val CLKOUT4 = out Bool ()
  val CLKOUT5 = out Bool ()
  val CLKOUT6 = out Bool ()

  val CLKFBIN = in Bool ()
  val CLKFBOUT = out Bool ()
  val CLKFBOUTB = out Bool ()

  val LOCKED = out Bool ()
  val RST = in Bool ()
  val PWRDWN = in Bool ()
}
