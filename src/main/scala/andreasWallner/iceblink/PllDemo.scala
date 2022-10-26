package andreasWallner.iceblink

import andreasWallner.blackbox.ice40.{SB_PLL40_CORE, SB_PLL40_PAD_CONFIG_EXT}
import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib.Delay
import spinal.lib.blackbox.lattice.ice40.SB_GB
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

import scala.language.postfixOps

case class PllDemo() extends Component {
  val io = new Bundle {
    val led = out(Bits(4 bit))
    val clk_debug = out(Bool())
    val lock = out(Bool())
  }

  val pll = SB_PLL40_CORE(
    SB_PLL40_PAD_CONFIG_EXT.make_config(clockDomain.frequency.getValue, 40 MHz)
  )
  pll.REFERENCECLK := clockDomain.readClockWire
  pll.BYPASS := False
  pll.RESETB := True
  //pll.LATCHINPUTVALUE := False
  io.lock := False //pll.LOCK
  val buffered = SB_GB(pll.PLLOUTGLOBAL)
  assert(
    clockDomain.hasResetSignal == false,
    "only boot reset is supported ATM"
  )
  io.clk_debug := pll.PLLOUTCORE
  val pllDomain =
    ClockDomain(buffered, frequency = FixedFrequency(20 MHz))

  val pllArea = new ClockingArea(pllDomain) {
    new SlowArea(1 Hz) {
      val counter = Reg(UInt(4 bit))
      counter := counter + 1
      io.led := counter.asBits
    }
  }
}

object PllDemo extends App {
  SpinalWarning("The PllDemo does currently not work correctly, PLL is stuck")
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(100.0 / 3.0 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(PllDemo())
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
    yosysPath = "/home/uhu01/Downloads/oss-cad-suite/bin/"
  )
  println(synth.getFMax())
  println(synth.getArea())
}
