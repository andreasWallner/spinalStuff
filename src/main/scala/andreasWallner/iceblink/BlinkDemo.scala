package andreasWallner.iceblink

import spinal.core._

case class BlinkDemo() extends Component {
  val io = new Bundle {
    val led = out Bits(4 bit)
  }

  val oneHzArea = new SlowArea(1 Hz) {
    val counter = Reg(UInt(4 bit))
    counter := counter + 1
    io.led := counter.asBits
  }
}

object BlinkDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind=BOOT),
    defaultClockDomainFrequency = FixedFrequency(3.33 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    val comp = BlinkDemo()
    comp.io.led.setName("led")
    comp
  }
}
