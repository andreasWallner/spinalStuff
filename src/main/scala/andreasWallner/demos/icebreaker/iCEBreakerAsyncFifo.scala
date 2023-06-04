package andreasWallner.demos.icebreaker

import andreasWallner.eda.YosysFlow
import spinal.core._
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

import scala.language.postfixOps

case class iCEBreakerAsyncFifoDemo() extends Component {
  val io = new iCEBreakerIO(GPIOPmod12(), GPIOPmod12(), GPIOPmod12())

  val x = new SlowArea(1 Hz) {
    val cnt = Reg(UInt(2 bit))
    cnt := cnt + 1
  }
  io.redLed := x.cnt(0)
  io.greenLed := io.userButton
}

object iCEBreakerAsyncFifoDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(iCEBreakerAsyncFifoDemo())
  }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "up5k",
    "sg48",
    frequencyTarget = Some(12 MHz),
    pcfFile = Some("../extras/iCEBreaker.pcf")
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  if (false)
    println("iceprog " + synth.bitstreamFile.get !!)
}
