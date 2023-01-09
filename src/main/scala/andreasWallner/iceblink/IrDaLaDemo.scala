package andreasWallner.iceblink

import andreasWallner.la.{AnalyzerGenerics, RLECompressor}
import andreasWallner.yosys.YosysFlow
import spinal.core._
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.{InOutWrapper, TriState, TriStateArray}
import spinal.lib._
import spinal.lib.blackbox.lattice.ice40.SB_GB

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
  }

  override def asMaster(): Unit = {
    out(leds, ledGreen, irda.tx, irda.sd_n, ftdi1.tx, ftdi1.cts, ftdi1.dsr, ftdi1.dcd)
    in(irda.rx, ftdi0.ss, ftdi0.mosi, ftdi0.sck, ftdi1.rx, ftdi1.dtr, ftdi1.rts)
    master(pmod, t, b, ftdi0.miso)

    leds.default(0)
    ledGreen.default(False)

  }

  def defaultConnections(): Unit = {
    if(!leds.hasAssignement)
      leds.clearAll()
    if (!ledGreen.hasAssignement) ledGreen := False

    if (!irda.tx.hasAssignement) irda.tx := True
    if (!irda.sd_n.hasAssignement) irda.sd_n := True

    // TODO Check for BitsBitAssignmentFixed and assign leftovers only (for leds also)
    if (!pmod.writeEnable.hasAssignement) {
      pmod.writeEnable.clearAll()
      pmod.write.clearAll()
    }
    if (!t.writeEnable.hasAssignement) {
      t.writeEnable.clearAll()
      t.write.clearAll()
    }
    if (!b.writeEnable.hasAssignement) {
      b.writeEnable.clearAll()
      b.write.clearAll()
    }

    if (!ftdi0.miso.writeEnable.hasAssignement) ftdi0.miso.writeEnable := False
    if (!ftdi0.miso.write.hasAssignement) ftdi0.miso.write := False

    if (!ftdi1.tx.hasAssignement) ftdi1.tx := True
    if (!ftdi1.cts.hasAssignement) ftdi1.cts := True
    if (!ftdi1.dsr.hasAssignement) ftdi1.dsr := True
    if (!ftdi1.dcd.hasAssignement) ftdi1.dcd := True
  }

  override def asSlave() = ???
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
  io.defaultConnections()
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
