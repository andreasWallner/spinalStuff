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

  override def asMaster() = {
    out(leds, ledGreen, irda.tx, irda.sd_n, ftdi1.tx, ftdi1.cts, ftdi1.dsr, ftdi1.dcd)
    in(irda.rx, ftdi0.ss, ftdi0.mosi, ftdi0.sck, ftdi1.rx, ftdi1.dtr, ftdi1.rts)
    master(pmod, t, b, ftdi0.miso)
  }

  def defaultConnections() = {
    for (i <- 0 until 4)
      if (leds(i).getSingleDriver.isEmpty) leds(i) := False
    if (ledGreen.getSingleDriver.isEmpty) ledGreen := False

    if (irda.tx.getSingleDriver.isEmpty) irda.tx := True
    if (irda.sd_n.getSingleDriver.isEmpty) irda.sd_n := True

    for (i <- 0 until 8) {
      if (pmod.writeEnable(i).getSingleDriver.isEmpty) {
        pmod.writeEnable(i) := False
        pmod.write(i) := False
      }
      if (t.writeEnable(i).getSingleDriver.isEmpty) {
        t.writeEnable(i) := False
        t.write(i) := False
      }
      if (b.writeEnable(i).getSingleDriver.isEmpty) {
        b.writeEnable(i) := False
        b.write(i) := False
      }
    }

    if (ftdi0.miso.writeEnable.getSingleDriver.isEmpty) ftdi0.miso.writeEnable := False
    if (ftdi0.miso.write.getSingleDriver.isEmpty) ftdi0.miso.write := False

    if (ftdi1.tx.getSingleDriver.isEmpty) ftdi1.tx := True
    if (ftdi1.cts.getSingleDriver.isEmpty) ftdi1.cts := True
    if (ftdi1.dsr.getSingleDriver.isEmpty) ftdi1.dsr := True
    if (ftdi1.dcd.getSingleDriver.isEmpty) ftdi1.dcd := True
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
