package andreasWallner.iceblink

import spinal.core._
import spinal.lib._
import spinal.lib.io.InOutWrapper

case class EppDemo () extends Component {
  val io = new Bundle {
    val epp = slave(EPP())
    val led = out Bits (4 bit)
  }

  val factory = new EppBusFactory(io.epp)
  io.led(0) := factory.createReadAndWrite(Bool(), 0, 0)
  io.led(1) := factory.createReadAndWrite(Bool(), 1, 0)
  io.led(2) := factory.createReadAndWrite(Bool(), 2, 0)
  io.led(3) := factory.createReadAndWrite(Bool(), 3, 0)
}

object EppDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind=BOOT),
    defaultClockDomainFrequency = FixedFrequency(3.33 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    val comp = EppDemo()
    comp.io.led.setName("led")
    InOutWrapper(comp)
  }
}
