package andreasWallner.demos

import andreasWallner.eda.YosysFlow
import andreasWallner.iceblink.IceStickIO
import andreasWallner.intro.{UartApbBridge, UartApbBridgeGenerics}
import andreasWallner.io.iso7816.S10MasterPeripheral
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

case class Iso7816S10Master() extends Component {
  val io = master port IceStickIO()

  val apbConfig = Apb3Config(addressWidth = 5, dataWidth = 8, useSlaveError = false)
  val bridge = UartApbBridge(UartApbBridgeGenerics(apbConfig))
  io.ftdi1.tx := bridge.io.uart.txd
  bridge.io.uart.rxd := io.ftdi1.rx

  val module = S10MasterPeripheral[Apb3](4, Apb3(apbConfig), Apb3SlaveFactory(_))
  module.io.bus <> bridge.io.apb
  io.leds(0) := io.pmod(4).read
  io.leds(1) := io.pmod(5).read
  io.leds(2) := io.pmod(6).read
  io.leds(3) := module.io.busy
  io.ledGreen := io.pmod(7).read

  io.pmod(4) <> module.io.iso.data
  io.pmod(5).write := module.io.iso.clk
  io.pmod(6).write := module.io.iso.rst
  io.pmod(7).write := module.io.vcc
  io.pmod(5).writeEnable := True
  io.pmod(6).writeEnable := True
  io.pmod(7).writeEnable := True

  io.pmod(0).write := io.pmod(4).read
  io.pmod(1).write := io.pmod(5).read
  io.pmod(2).write := io.pmod(6).read
  io.pmod(3).write := io.pmod(7).read
  for(i <- 0 until 4) {
    io.pmod(i).write := False
    io.pmod(i).writeEnable := True
  }
}

object Iso7816S10Master extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(Iso7816S10Master(), InOutWrapper.LatticeIce40SB_IO)
  }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(12 MHz),
    pcfFile = Some("icestick.pcf"),
    yosysPath = "/opt/oss-cad-suite-20230926/bin/"
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  if(args.contains("prog"))
    println(s"iceprog ${synth.bitstreamFile.get}"!!)
}
