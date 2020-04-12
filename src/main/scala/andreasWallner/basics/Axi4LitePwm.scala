package andreasWallner.basics;

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{
  AxiLite4,
  AxiLite4Config,
  AxiLite4SlaveFactory
}

case class AxiLite4Pwm(
    generics: PwmGenerics,
    axi_config: AxiLite4Config =
      AxiLite4Config(addressWidth = 7, dataWidth = 32)
) extends Component {
  val io = new Bundle {
    val axilite = slave(AxiLite4(axi_config))
    val pwms = out Vec (Bool, generics.channelCnt)
  }

  val pwmCtrl = new PwmCtrl(generics)
  io.pwms <> pwmCtrl.io.pwms

  val busCtrl = new AxiLite4SlaveFactory(io.axilite)
  val bridge = pwmCtrl.driveFrom(busCtrl, 0)
}
