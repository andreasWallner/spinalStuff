package andreasWallner.iceblink

import andreasWallner.blackbox.ice40.{SB_PLL40_CORE, SB_PLL40_PAD_CONFIG_EXT}
import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib.{Counter, Delay, master}
import spinal.lib.blackbox.lattice.ice40.{SB_GB, SB_PLL40_PAD}
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

import scala.language.postfixOps

case class PllDemo() extends Component {
  val io = new Bundle {
    val led = out(Bits(4 bit))
    //val clk_debug = out(Bool())
    val lock = out(Bool())
  }
  assert(!clockDomain.hasResetSignal, "only boot reset is supported ATM")

  val pllConfig = SB_PLL40_PAD_CONFIG_EXT.make_config(clockDomain.frequency.getValue, 40 MHz)
  val pll = SB_PLL40_CORE(pllConfig, withLock = true)
  println(pllConfig)
  pll.REFERENCECLK := clockDomain.readClockWire
  pll.BYPASS := False
  pll.RESETB := True
  //pll.LATCHINPUTVALUE := False
  io.lock := True //pll.LOCK
  val buffered = SB_GB(pll.PLLOUTCORE)
  //io.clk_debug := pll.PLLOUTCORE
  val pllDomain =
    ClockDomain(buffered, frequency = FixedFrequency(40 MHz))

  io.led(3) := pll.LOCK

  val pllArea = new ClockingArea(pllDomain) {
    new SlowArea(1 Hz) {
      val counter = Reg(UInt(4 bit))
      counter := counter + 1
      io.led(2 downto 0) := counter.asBits(2 downto 0)
    }
  }
}

case class SB_DFFR() extends BlackBox {
  val D = in(Bool())
  val C = in(Bool())
  val R = in(Bool())
  val Q = out(Bool())
}
object SB_DFFR {
  def apply(D: Bool, C: Bool, R: Bool): Bool = {
    val ff = SB_DFFR()
    ff.D := D
    ff.C := C
    ff.R := R
    ff.Q
  }
}

object Foo {
  def PllDomain(
      fout: HertzNumber,
      cd: ClockDomain = null,
      outputBuffer: Boolean = true,
      corePll: Boolean = true,
      useLockedAsReset: Boolean = false
  ): ClockDomain = {
    val PllArea = new Area {
      val sourceDomain = Option(cd).getOrElse(ClockDomain.current)
      assert(
        !sourceDomain.hasClockEnableSignal,
        "Can't use ClockDomain with enable as source for PLL"
      )
      assert(
        sourceDomain.frequency.isInstanceOf[FixedFrequency],
        "Can't calculate PLL if source frequency is not fixed"
      )

      val config = SB_PLL40_PAD_CONFIG_EXT.make_config(sourceDomain.frequency.getValue, fout)
      val pll = if (corePll) {
        andreasWallner.blackbox.ice40.SB_PLL40_CORE(config, withLock = useLockedAsReset)
      } else {
        andreasWallner.blackbox.ice40.SB_PLL40_PAD(config, withLock = useLockedAsReset)
      }

      pll.clockInput := sourceDomain.readClockWire
      pll.RESETB := True
      pll.BYPASS := False

      val pllOutput = if (outputBuffer) SB_GB(pll.PLLOUTGLOBAL) else pll.PLLOUTCORE

      val lockedOrTrue = if (useLockedAsReset) pll.LOCK else True
      // TODO instantiate a LUT here to prevent LUT combining in the future
      val asyncReset =
        (sourceDomain.isResetActive ## sourceDomain.isSoftResetActive ## !lockedOrTrue).orR

      val reset =
        if (sourceDomain.hasResetSignal || sourceDomain.hasSoftResetSignal || !useLockedAsReset) {
          None
        } else {
          val resetSync0 = SB_DFFR(D = True, C = pllOutput, R = asyncReset)
          val resetSync1 = SB_DFFR(D = resetSync0, C = pllOutput, R = asyncReset)
          Some(!resetSync1)
        }
    }
    ClockDomain(
      clock = PllArea.pllOutput,
      reset = PllArea.reset.orNull,
      frequency = FixedFrequency(fout),
      config = ClockDomainConfig(
        resetKind = if (!useLockedAsReset) BOOT else ASYNC,
        resetActiveLevel = HIGH
      )
    )
  }
}

case class PllDemoIceStick() extends Component {
  val io = master(IceStickIO())

  val pllArea = new ClockingArea(Foo.PllDomain(80 MHz, useLockedAsReset = true)) {
    val oneHz = new SlowArea(1 Hz) {
      val counter = Reg(UInt(4 bit))
      counter := counter + 1
    }
  }
  io.leds := pllArea.oneHz.counter.asBits
  io.ledGreen := True

  io.defaultConnections()
}

object PllDemoIceStick extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(PllDemoIceStick())
  }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(40 MHz),
    pcfFile = Some("icestick.pcf"),
    yosysPath = "/opt/oss-cad-suite-20230105/bin/",
    nextpnrPath = "/opt/oss-cad-suite-20230105/bin/",
    icestormPath = "/opt/oss-cad-suite-20230105/bin/",
    verbose = true
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  //println(s"iceprog ${synth.bitstreamFile}" !!)
}

object PllDemo extends App {
  SpinalWarning("The PllDemo does currently not work correctly, PLL is stuck")
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(33 MHz),
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
    frequencyTarget = Some(40 MHz),
    pcfFile = Some("iceblink40.pcf"),
    allowUnconstrained = true,
    yosysPath = "/opt/oss-cad-suite-20230105/bin/",
    nextpnrPath = "/home/uhu01/git/nextpnr/",
    icestormPath = "/opt/oss-cad-suite-20230105/bin/",
    verbose = true
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  println("iceblink_demo/.env/bin/python -m iCEburn.__main__ -v -ew iceblink_demo/PllDemo.bin" !!)
}
