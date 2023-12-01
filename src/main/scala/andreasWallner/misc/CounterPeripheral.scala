package andreasWallner.misc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

/** Simple peripheral containing one bus-wide free-running counter
  *
  * Intended to measure access performance via linux /dev/mem & UIO
 **/
case class CounterPeripheral[T <: Data with IMasterSlave](
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component {
  val io = new Bundle {
    val bus = slave port busType()
  }
  val factory = metaFactory(io.bus)

  val counter = Reg(UInt(factory.busDataWidth bit)) init 0
  counter := counter + 1

  factory.readAndWrite(counter, 0, 0, "Free running counter")
}

object Axi4CounterPeripheral extends App {
  import spinal.lib.bus.amba4.axi._
  import andreasWallner.xilinx.XilinxNamer

  val config = Axi4Config(8, 32, idWidth=2)

  val report = SpinalConfig(
    targetDirectory = "demo",
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    device = Device.XILINX
  ).generateVerilog {
    XilinxNamer(CounterPeripheral(Axi4(config), Axi4SlaveFactory(_)))
  }
}

object Axi4LiteCounterPeripheral extends App {
  import spinal.lib.bus.amba4.axilite._
  import andreasWallner.xilinx.XilinxNamer

  val config = AxiLite4Config(8, 32)

  val report = SpinalConfig(
    targetDirectory = "demo",
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    device = Device.XILINX
  ).generateVerilog {
    XilinxNamer(CounterPeripheral(AxiLite4(config), AxiLite4SlaveFactory(_)))
  }
}
