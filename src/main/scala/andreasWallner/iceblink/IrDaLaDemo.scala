package andreasWallner.iceblink

import andreasWallner.la.{AnalyzerGenerics, RLECompressor}
import andreasWallner.eda.YosysFlow
import spinal.core._
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.{InOutWrapper, TriState, TriStateArray}
import spinal.lib._
import spinal.lib.blackbox.lattice.ice40.SB_GB
import spinal.lib.com.uart.Uart

import scala.language.postfixOps

case class IceStickIO() extends Bundle with IMasterSlave {
  val leds = Bits(4 bit)
  val ledGreen = Bool()
  val irda = new Bundle {
    val rx = Bool()
    val tx = Bool()
    val sd_n = Bool()
  }
  val pmod = TriStateArray(8 bit)
  val t = TriStateArray(8 bit)
  val b = TriStateArray(8 bit)

  val ftdi0 = new Bundle {
    val ss = Bool()
    val miso = TriState(Bool())
    val mosi = Bool()
    val sck = Bool()
  }

  val ftdi1 = new Bundle {
    val rx = new Bool()
    val tx = new Bool()
    val rts = new Bool()
    val cts = new Bool()
    val dtr = new Bool()
    val dsr = new Bool()
    val dcd = new Bool()

    def asUart(ctsGen: Boolean = false, rtsGen: Boolean = false) = {
      assert(rx.isInput, "asUart can only be used if IceStickIO is a master port")
      val uart = master(Uart(ctsGen, rtsGen))
      uart.rxd := rx
      tx := uart.txd
      if (ctsGen) uart.cts := cts
      if (rtsGen) rts := uart.rts
      uart
    }
  }

  override def asMaster(): Unit = {
    out(leds, ledGreen, irda.tx, irda.sd_n, ftdi1.tx, ftdi1.cts, ftdi1.dsr, ftdi1.dcd)
    in(irda.rx, ftdi0.ss, ftdi0.mosi, ftdi0.sck, ftdi1.rx, ftdi1.dtr, ftdi1.rts)
    master(pmod, t, b, ftdi0.miso)

    leds.default(0)
    ledGreen.default(False)
    irda.tx.default(True)
    irda.sd_n.default(True)

    pmod.writeEnable.default(0)
    pmod.write.default(0)
    t.writeEnable.default(0)
    t.write.default(0)
    b.writeEnable.default(0)
    b.write.default(0)

    ftdi0.miso.writeEnable.default(False)
    ftdi0.miso.write.default(False)

    ftdi1.tx.default(True)
    ftdi1.cts.default(True)
    ftdi1.dsr.default(True)
    ftdi1.dcd.default(True)
  }

  override def asSlave(): Unit = ???
}

case class IrDaLaDemo() extends Component {
  val io = master(IceStickIO())

  val sb = SB_GB(clockDomain.readClockWire)

  val drivenClock = new ClockingArea(new ClockDomain(sb, frequency = clockDomain.frequency)) {
    val slowed = new SlowArea(1 Hz) {
      val counter = Reg(UInt(4 bit))
      counter := counter + 1
      io.leds := counter.asBits
    }

    val toTrace = io.irda.rx.asBits
    val syncedTrace = BufferCC(toTrace)
    val compressor = RLECompressor(
      AnalyzerGenerics(dataWidth = toTrace.getWidth, internalWidth = 14)
    )
    compressor.io.data := syncedTrace
    compressor.io.run := Delay(True, 10, init = False)
    val overflow = Bool()
    val buffered = compressor.io.compressed.toStream(overflow, 128, 128)
    val uart = UartTransmitter(115200)
    buffered
      .fragmentTransaction(7)
      .translateInto(uart.io.data)((o, i) => o := i.last ## i.fragment)
    io.irda.sd_n := False
    io.ftdi1.tx := uart.io.tx
  }
}

object IrDaLaDemo extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    InOutWrapper(IrDaLaDemo())
  }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(100 MHz),
    pcfFile = Some("icestick.pcf")
  )
  println(synth.getFMax())
  println(synth.getArea())
}
