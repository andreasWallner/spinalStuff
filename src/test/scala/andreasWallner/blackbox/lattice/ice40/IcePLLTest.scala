package andreasWallner.blackbox.lattice.ice40

import spinal.core._
import spinal.lib.blackbox.lattice.ice40.SB_GB
import andreasWallner.blackbox.ice40._
import andreasWallner.eda.YosysFlow
import andreasWallner.iceblink.IceStickIO
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper
import spinal.lib._

import scala.language.postfixOps

class IcePLLTest(target: HertzNumber, feedbackPath: FeedbackPath) extends Component {
  val io = master port IceStickIO()

  val pllConfig =
    SB_PLL40_PAD_CONFIG_EXT.singleOutput(
      ClockDomain.current.frequency.getValue,
      target,
      FEEDBACK_PATH = Option(feedbackPath),
      PLLOUT_SELECT =
        if (feedbackPath == FeedbackPath.PHASE_AND_DELAY) PllOutSelect.SHIFTREG_0deg
        else PllOutSelect.GENCLK
    )
  println(pllConfig)
  val pll = SB_PLL40_CORE(pllConfig, withLock = true)
  pll.REFERENCECLK := clockDomain.readClockWire
  pll.BYPASS := False
  pll.RESETB := True
  val buffered = SB_GB(pll.PLLOUTGLOBAL)
  val pllDomain = ClockDomain(buffered, frequency = FixedFrequency(target))
  new ClockingArea(pllDomain) {
    new SlowArea(1 Hz) {
      val counter = Reg(UInt(4 bit))
      counter := counter + 1
      io.leds := counter.asBits
    }
  }
  io.ledGreen := pll.LOCK
}

class IcePLLTestGen(target: HertzNumber, download: Boolean, feedbackPath: FeedbackPath = null)
    extends App {
  import andreasWallner.rtlutils.ComponentPimper

  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(new IcePLLTest(target, feedbackPath))
  }
  val maxClock = report.toplevel
    .getAllClockDomains()
    .map(cd => cd.frequency.getValue)
    .max(Ordering.by[HertzNumber, BigDecimal](hn => hn.toBigDecimal))

  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(maxClock),
    pcfFile = Some("icestick.pcf"),
    yosysPath = "/opt/oss-cad-suite-20230105/bin/",
    nextpnrPath = "/opt/oss-cad-suite-20230105/bin/",
    icestormPath = "/opt/oss-cad-suite-20230105/bin/",
    verbose = true
  )

  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  if (download)
    println(s"iceprog ${synth.bitstreamFile.get}" !!)
}

object IcePLLTest100M extends IcePLLTestGen(96 MHz, true)
object IcePLLTest40M extends IcePLLTestGen(40 MHz, true)
object IcePLLTest200M extends IcePLLTestGen(170 MHz, true)
object IcePLLTest40MDELAY extends IcePLLTestGen(36 MHz, true, FeedbackPath.DELAY)
object IcePLLTest40MPHASEDELAY extends IcePLLTestGen(36 MHz, true, FeedbackPath.PHASE_AND_DELAY)
