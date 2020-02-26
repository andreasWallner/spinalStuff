package innovative_solutions.basics;

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{
  AxiLite4,
  AxiLite4Config,
  AxiLite4SlaveFactory
}

object AxiLite4Pwm {
  def getAxi4Config = AxiLite4Config(5, 32)
}

case class AxiLite4Pwm(width: Int) extends Component {
  val io = new Bundle {
    val axilite = slave(AxiLite4(AxiLite4Pwm.getAxi4Config))
    val pwm = out Bool
  }

  val pwmCtrl = new PwmCtrl(width)
  io.pwm <> pwmCtrl.io.pwm

  val busCtrl = new AxiLite4SlaveFactory(io.axilite)
  val bridge = pwmCtrl.driveFrom(busCtrl, 0)
}
