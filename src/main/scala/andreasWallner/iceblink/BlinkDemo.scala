package andreasWallner.iceblink

import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib.eda.bench.Rtl

import scala.language.postfixOps

case class BlinkDemo() extends Component {
  val io = new Bundle {
    val led = out Bits (4 bit)
  }

  val oneHzArea = new SlowArea(1 Hz, allowRounding = true) {
    val counter = Reg(UInt(4 bit))
    counter := counter + 1
    io.led := counter.asBits
  }
}

object BlinkDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(100.0 / 3.0 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    BlinkDemo()
  }
  val synth = YosysFlow(
    "iceblink_demo",
    Rtl(report),
    family = "ice40",
    device = "lp1k",
    fpgaPackage = "qn84",
    frequencyTarget = Some(100 MHz),
    pcfFile = Some("iceblink40.pcf"),
    allowUnconstrained = true,
    yosysPath = "/opt/oss-cad-suite-20230105/bin/",
    nextpnrPath = "/home/uhu01/git/nextpnr/",
    icestormPath = "/opt/oss-cad-suite-20230105/bin/"
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  println("iceblink_demo/.env/bin/python -m iCEburn.__main__ -v -ew iceblink_demo/BlinkDemo.bin" !!)
}
