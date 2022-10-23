package andreasWallner.iceblink

import andreasWallner.la.{AnalyzerGenerics, RLECompressor}
import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper
import spinal.lib._

import scala.language.postfixOps

object UartTransmitter {
  def apply[T <: Data](baudrate: Int, data: Stream[T]): Bool = {
    val payload = data.payload match {
      case b: Bits => b
      case f: Fragment[Bits] => f.fragment
    }
    assert(payload.getWidth == 8, "Uart can only send 8 bit wide data")
    val module = UartTransmitter(baudrate)
    module.io.data.arbitrationFrom(data)
    module.io.data.payload := payload
    module.io.tx
  }
}

case class UartTransmitter(baudrate: Int) extends Component {
  val io = new Bundle {
    val tx = out Bool()
    val data = slave(Stream(Bits(8 bit)))
  }

  val timing = new Area {
    val count = (clockDomain.frequency.getValue.toBigDecimal / baudrate).toBigInt() - 1
    val counter = Reg(UInt(log2Up(count) bits))

    val en = Bool()
    val strobe = counter === 0
    when(!en || strobe) {
      counter := count
    } otherwise {
      counter := counter - 1
    }
  }

  val toSend = Reg(Bits(10 bit)) init 0x01
  io.tx := toSend(0)
  val transmitting = Reg(Bool()) init False
  timing.en := transmitting
  io.data.ready := !transmitting
  when(transmitting) {
    when(timing.strobe) {
      when(toSend === 0x01) {
        transmitting := False
      } otherwise {
        toSend := toSend |>> 1
      }
    }
  } elsewhen (io.data.valid) {
    toSend := True ## io.data.payload ## False
    transmitting := True
  }
}

case class EppLaDemo() extends Component {
  val io = new Bundle {
    val tx = out Bool()
    val tx2 = out Bool()
    val epp = slave(EPP())
    val led = out Bits (4 bit)
  }
  io.tx2 := io.tx

  val factory = new EppBusFactory(io.epp)
  io.led(0) := factory.createReadAndWrite(Bool(), 0, 0)
  io.led(1) := factory.createReadAndWrite(Bool(), 1, 0)
  io.led(2) := factory.createReadAndWrite(Bool(), 2, 0)
  io.led(3) := factory.createReadAndWrite(Bool(), 3, 0)

  val toTrace = io.epp.ASTB ## io.epp.DSTB ## io.epp.WAIT ## io.epp.DB.writeEnable ## io.epp.DB.read
  val syncedTrace = BufferCC(toTrace)
  val compressor = RLECompressor(AnalyzerGenerics(dataWidth = 12, internalWidth = 14))
  compressor.io.data := syncedTrace
  compressor.io.run := Delay(True, 10, init = False)
  val overflow = Bool()
  val buffered = compressor.io.compressed.toStream(overflow, 128, 128)
  val bufferedBits = buffered.payload.asBits
  val padded = buffered.translateWith(True ## bufferedBits(13 downto 7) ## False ## bufferedBits(6 downto 0))
  val fragmented = padded.fragmentTransaction(8)
  io.tx := UartTransmitter(115200, fragmented)
}

object EppLaDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(3.33 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    val comp = EppLaDemo()
    comp.io.led.setName("led")
    InOutWrapper(comp)
  }
  val synth = YosysFlow("yosys", "iceblink_demo", Rtl(report), "ice40", "lp1k", "qn84", Some(100 MHz), Some("iceblink40.pcf"))
  println(synth.getFMax())
  println(synth.getArea())
}
