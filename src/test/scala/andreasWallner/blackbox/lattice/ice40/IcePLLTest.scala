/*
package andreasWallner.blackbox.lattice.ice40

import spinal.core._
import spinal.lib.blackbox.lattice.ice40._
import andreasWallner.eda.YosysFlow
import andreasWallner.iceblink.IceStickIO
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper
import spinal.lib._

import scala.language.postfixOps

class IcePLLTest(target: HertzNumber, feedbackPath: FeedbackPath) extends Component {
  val io = master port IceStickIO()

  val pllConfig =
    SB_PLL40_CONFIG.singleOutput(
      ClockDomain.current.frequency.getValue,
      target,
      FEEDBACK_PATH = Option(feedbackPath),
      PLLOUT_SELECT =
        if (feedbackPath == FeedbackPath.PHASE_AND_DELAY) PllOutSelect.SHIFTREG_0deg
        else PllOutSelect.GENCLK,
      withLock = true
    )
  println(pllConfig)
  val pll = SB_PLL40_CORE(pllConfig)
  pll.REFERENCECLK := clockDomain.readClockWire
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

class IcePLLTestMakePll(target: HertzNumber) extends Component {
  val io = master port IceStickIO()

  val resettedCD = ClockDomain.current.copy(
    reset = True,
    config = ClockDomain.current.config.copy(resetKind = SYNC, resetActiveLevel = LOW)
  )

  new ClockingArea(resettedCD) {
    new ClockingArea(ICE40_PLL.makePLL(target)) {
      new SlowArea(1 Hz) {
        val counter = Reg(UInt(4 bit))
        counter := counter + 1
        io.leds := counter.asBits
      }
    }
  }
}

class IcePLLTestGen(target: => Component, download: Boolean) extends App {
  import andreasWallner.rtlutils.ComponentPimper

  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(target)
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

object IcePLLTest96M extends IcePLLTestGen(new IcePLLTest(96 MHz, FeedbackPath.SIMPLE), true)
object IcePLLTest40M extends IcePLLTestGen(new IcePLLTest(40 MHz, FeedbackPath.SIMPLE), true)
object IcePLLTest170M extends IcePLLTestGen(new IcePLLTest(170 MHz, FeedbackPath.SIMPLE), true)
object IcePLLTest36MDELAY extends IcePLLTestGen(new IcePLLTest(36 MHz, FeedbackPath.DELAY), true)
object IcePLLTest36MPHASEDELAY
    extends IcePLLTestGen(new IcePLLTest(36 MHz, FeedbackPath.PHASE_AND_DELAY), true)

object IcePLLMake96M extends IcePLLTestGen(new IcePLLTestMakePll(96 MHz), true)
*/
