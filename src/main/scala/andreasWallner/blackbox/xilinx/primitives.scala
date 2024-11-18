package andreasWallner.blackbox.xilinx

import spinal.core._

// see https://www.xilinx.com/support/documentation/sw_manuals/xilinx13_2/7series_scm.pdf

object FDP {
  def apply(C: Bool, D: Bool, PRE: Bool): Bool = {
    val ff = FDP()
    ff.C := C
    ff.D := D
    ff.PRE := PRE
    ff.Q
  }
}

case class FDP() extends BlackBox {
  val C = in Bool()
  val D = in Bool()
  val PRE = in Bool()

  val Q = out Bool()
}

object FDPE {
  def apply(C: Bool, CE: Bool, D: Bool, PRE: Bool): Bool = {
    val ff = FDPE()
    ff.C := C
    ff.CE := CE
    ff.D := D
    ff.PRE := PRE
    ff.Q
  }
}

case class FDPE() extends BlackBox {
  val C = in Bool()
  val CE = in Bool()
  val D = in Bool()
  val PRE = in Bool()

  val Q = out Bool()
}

object FDC {
  def apply(C: Bool, D: Bool, CLR: Bool): Bool = {
    val ff = FDC()
    ff.C := C
    ff.D := D
    ff.CLR := CLR
    ff.Q
  }
}

case class FDC() extends BlackBox {
  val C = in Bool()
  val D = in Bool()
  val CLR = in Bool()

  val Q = out Bool()
}

object FDCE {
  def apply(C: Bool, CE: Bool, D: Bool, CLR: Bool): Bool = {
    val ff = FDCE()
    ff.C := C
    ff.CE := CE
    ff.D := D
    ff.CLR := CLR
    ff.Q
  }
}

case class FDCE() extends BlackBox {
  val C = in Bool()
  val CE = in Bool()
  val D = in Bool()
  val CLR = in Bool()

  val Q = out Bool()
}

// see https://docs.amd.com/r/en-US/ug953-vivado-7series-libraries/DNA_PORT

case class DNA_PORT(simDnaValue: Option[BigInt]) extends BlackBox {
  simDnaValue.foreach(v => addGeneric("SIM_DNA_VALUE", v))
  val DIN = in port Bool()
  val READ = in port Bool()
  val SHIFT = in port Bool()
  val DOUT = out port Bool()
  val CLK = in port Bool()

  mapCurrentClockDomain(CLK)
}
