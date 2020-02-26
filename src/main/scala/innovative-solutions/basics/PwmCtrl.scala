package innovative_solutions.basics

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}

class PwmCtrl(width: Int) extends Component {
  val io = new Bundle {
    val max_level = in UInt(width bits)
    val level = in UInt(width bits)
    val pwm = out Bool
  }

  val pwm = new Pwm(width)
  pwm.io.pwm <> io.pwm
  pwm.io.max_level <> io.max_level
  pwm.io.level <> io.level

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: Int = 0) = new Area {
    require(busCtrl.busDataWidth > width)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    busCtrlWrapped.driveAndRead(io.max_level, address=0) init(0)
    busCtrlWrapped.driveAndRead(io.level, address=4) init(0)
  }
}