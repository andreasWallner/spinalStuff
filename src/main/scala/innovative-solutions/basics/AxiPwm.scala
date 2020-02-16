package uhu01.basics;

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SlaveFactory}

object Axi4Pwm {
  def getAxi4Config = Axi4Config {

  }
}

case class Axi4Pwm(width : Int) extends Component {
  val io = new Bundle {
    val axi = slave Axi4(Axi4Pwm.getAxi4Config)
    val pwm = out Bool
  }

  val pwmCore = new Pwm(width)
  pwm := pwmCore.io.pwm

  val busCtrl = Axi4SlaveFactory(io.axi)
  busCtrl.createReadAndWrite(UInt, address = 0, 0) init(0)
}