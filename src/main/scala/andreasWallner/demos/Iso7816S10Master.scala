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
  io.ledGreen := module.io.busy
  io.leds(0) := io.pmod(0).read
  io.leds(3 downto 1) := 0

  io.pmod(0) <> module.io.iso.data
  io.pmod(1).write := module.io.iso.clk
  io.pmod(2).write := module.io.iso.rst
  io.pmod(3).write := module.io.vcc
  io.pmod(1).writeEnable := True
  io.pmod(2).writeEnable := True
  io.pmod(3).writeEnable := True

  io.pmod(4).write := module.by1024.core.io.rsp.valid.pull()
  io.pmod(4).writeEnable := True
  io.pmod(5).write := module.by1024.core.io.rsp.payload.pull()
  io.pmod(5).writeEnable := True
  io.pmod(6).write := module.rspFifo.io.push.valid.pull()
  io.pmod(6).writeEnable := True
  for(i <- 7 until 8) {
    io.pmod(i).write := False
    io.pmod(i).writeEnable := False
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
  if(args.contains("prog") || true)
    println(s"iceprog ${synth.bitstreamFile.get}"!!)
}
