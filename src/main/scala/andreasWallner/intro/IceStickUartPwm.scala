package andreasWallner.intro

import andreasWallner.iceblink.IceStickIO
import andreasWallner.io.pwm.Apb3Pwm
import andreasWallner.io.pwm.Pwm.{CoreParameters, PeripheralParameters}
import andreasWallner.registers.datamodel.{Bus, BusElement}
import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

import scala.language.postfixOps

class IceStickUartPwm extends Component with Bus {
  val io = master(IceStickIO())

  val apbConfig = Apb3Config(addressWidth = 5, dataWidth = 16, useSlaveError = false)

  val bridge = UartApbBridge(UartApbBridgeGenerics(apbConfig)).io
  bridge.uart.rxd := io.ftdi1.rx
  io.ftdi1.tx := bridge.uart.txd
  // TODO bridge.uart <> io.ftdi1.asUart

  val pwm = Apb3Pwm(
    PeripheralParameters(CoreParameters(counterWidth = 8, channelCnt = 5), dividerWidth = 8),
    apbConfig
  )
  bridge.apb <> pwm.io.bus
  for(i <- 0 until 4)
    io.leds(i) := pwm.io.pwm(i)
  io.ledGreen := pwm.io.pwm(4)

  def elements: Iterable[(BusElement, Long)] = Seq((pwm, 0))
}

object IceStickUartPwm extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(new IceStickUartPwm)
  }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(12 MHz),
    pcfFile = Some("icestick.pcf")
  )
  println(HertzNumber(synth.getFMax()).decompose)
  println(synth.getArea())

  import scala.sys.process._
  if(args.contains("prog"))
    println(s"iceprog ${synth.bitstreamFile.get}" !!)
}
