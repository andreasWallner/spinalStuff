package innovative_solutions.basics

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}

class PwmCtrl(generics: PwmGenerics) extends Component {
  val io = new Bundle {
    val max_count = in UInt (generics.counterWidth bits)
    val levels = in Vec (UInt(generics.counterWidth bits), generics.channelCnt)
    val pwms = out Vec (Bool, generics.channelCnt)
  }

  val pwm = new Pwm(generics)
  pwm.io.pwms <> io.pwms
  pwm.io.max_count <> io.max_count
  pwm.io.levels <> io.levels

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: Int = 0) = new Area {
    require(busCtrl.busDataWidth >= generics.counterWidth)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    busCtrlWrapped.driveAndRead(io.max_count, address = 0) init (0)
    io.levels.zipWithIndex.foreach { case (level, index) =>
      busCtrlWrapped.driveAndRead(level, address = 4 * (index + 1)) init (0)
    }
  }
}
