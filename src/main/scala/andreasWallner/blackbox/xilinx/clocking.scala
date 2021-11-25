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
