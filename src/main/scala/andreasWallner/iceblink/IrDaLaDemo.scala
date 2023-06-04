package andreasWallner.iceblink

import andreasWallner.demos.RgbLed
import andreasWallner.la.{AnalyzerGenerics, RLECompressor}
import andreasWallner.eda.YosysFlow
import andreasWallner.io.spi.Spi
import spinal.core.{Bool, _}
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.{InOutWrapper, TriState, TriStateArray}
import spinal.lib._
import spinal.lib.blackbox.lattice.ice40.SB_GB
import spinal.lib.com.uart.Uart

import scala.language.postfixOps

// PMOD Reference: https://digilent.com/reference/_media/reference/pmod/pmod-interface-specification-1_3_1.pdf

abstract class BasePmodSpi extends Bundle {
  val spi = Spi()

  def connectSpi(cs: TriState[Bool], mosi: TriState[Bool], miso: TriState[Bool], sck: TriState[Bool]): Unit = {
    cs.write := spi.cs
    cs.writeEnable := True
    mosi.write := spi.mosi
    mosi.writeEnable := True
    miso.write.assignDontCare()
    miso.writeEnable := False
    spi.miso := miso.read
    sck.write := spi.sclk
    sck.writeEnable := True
  }
}

case class Pmod6Spi() extends BasePmodSpi

case class Pmod12Spi(withInt: Boolean, withReset: Boolean, csCount: Int) extends BasePmodSpi {
  val int = if(withInt) Some(Bool()) else None
  val reset = if(withReset) Some(Bool()) else None

  val gpio7 = if(withInt) None else Some(TriState(Bool()))
  val gpio8 = if(withReset) None else Some(TriState(Bool()))
  val gpio9 = if(csCount > 1) None else Some(TriState(Bool()))
  val gpio10 = if(csCount > 2) None else Some(TriState(Bool()))
}

abstract class BasePmodUart(withFlowControl: Boolean=false) extends Bundle {
  val uart = new Uart(ctsGen=withFlowControl, rtsGen=withFlowControl)
  val gpio1 = if(withFlowControl) None else Some(TriState(Bool()))
  val gpio4 = if(withFlowControl) None else Some(TriState(Bool()))

  def connect(tx: TriState[Bool], rx: TriState[Bool], cts: TriState[Bool], rts: TriState[Bool]) = {
    tx.write := uart.txd
    tx.writeEnable := True
    uart.rxd := rx.read
    rx.writeEnable := False
    rx.write.assignDontCare()

    if(withFlowControl) {
      uart.cts := cts.read
      cts.writeEnable := True
      cts.write.assignDontCare()

      rts.write := uart.rts
      rts.writeEnable := True
    } else {
      gpio1.get <> cts
      gpio1.get.write.default(False)
      gpio1.get.writeEnable.default(False)

      gpio4.get.write.default(False)
      gpio4.get.writeEnable.default(False)
    }
  }
}
case class Pmod6Uart(withFlowControl: Boolean=false) extends BasePmodUart(withFlowControl)
case class Pmod12Uart(withFlowControl: Boolean=false, withInt: Boolean=false, withReset: Boolean=false) extends BasePmodUart(withFlowControl) {
  val int = if (withInt) Some(Bool()) else None
  val reset = if (withReset) Some(Bool()) else None

  val gpio7 = if (withInt) None else Some(TriState(Bool()))
  val gpio8 = if (withReset) None else Some(TriState(Bool()))
  val gpio9 = TriState(Bool())
  val gpio10 = TriState(Bool())
}

case class PMOD6() extends Bundle {
  val io = Vec(TriState(Bool()), 4)
  for((io, idx) <- io.zipWithIndex) {
    master(io)
    io.setName(f"pin${idx+1}")
    io.writeEnable.default(False)
    io.write.default(False)
  }

  def asSpiMaster(doRename: Boolean=false) = {
    val bundle = Pmod6Spi()
    bundle.connectSpi(io(0), io(1), io(2), io(3))
    if(doRename) {
      io(0).setName("cs")
      io(1).setName("mosi")
      io(2).setName("miso")
      io(3).setName("sck")
    }
    bundle
  }

  def asUartMaster() = {
    val bundle = Pmod6Uart()
    bundle.connect(io(1), io(2), io(0), io(3))
    bundle
  }
}
case class PMOD12() extends Bundle {
  val io = Vec(TriState(Bool()), 8)
  for ((io, idx) <- io.zipWithIndex) {
    master(io)
    io.writeEnable.default(False)
    io.write.default(False)
    io.setName(if(idx <= 4) f"pin${idx + 1}" else f"pin${idx + 3}")
  }

  def asSpiMaster(withInt: Boolean=false, withReset: Boolean=false, csCount: Int=1, doRename: Boolean = false) = {
    assert(csCount < 3, "PMOD officially only supports 3 CS")
    assert(csCount < 2, "SPI bundle currently only supports 1 CS...")

    val bundle = Pmod12Spi(withInt, withReset, csCount)
    bundle.connectSpi(io(0), io(1), io(2), io(3))

    if(withInt) {
      io(4).write.assignDontCare()
      io(4).writeEnable := False
      bundle.int.get := io(4).read
    } else {
      bundle.gpio7.get <> io(4)
      bundle.gpio7.get.write.default(False)
      bundle.gpio7.get.writeEnable.default(False)
    }

    if(withReset) {
      io(5).write := bundle.reset.get
      io(5).writeEnable := True
    } else {
      bundle.gpio8.get <> io(5)
      bundle.gpio8.get.write.default(False)
      bundle.gpio8.get.writeEnable.default(False)
    }

    if(csCount > 1) {
      io(6).write := True // TODO
      io(6).writeEnable := True
    } else {
      bundle.gpio9.get <> io(6)
      bundle.gpio9.get.write.assignDontCare()
      bundle.gpio9.get.writeEnable.default(False)
    }

    if(csCount > 2) {
      io(7).write := True // TODO
      io(7).writeEnable := True
    } else {
      bundle.gpio10.get <> io(7)
      bundle.gpio10.get.write.assignDontCare()
      bundle.gpio10.get.writeEnable.default(False)
    }

    if(doRename) {
      io(0).setName("cs")
      io(1).setName("mosi")
      io(2).setName("miso")
      io(3).setName("sck")
      if(withInt) io(4).setName("int")
      if(withReset) io(5).setName("reset")
      if(csCount > 1) io(6).setName("cs2")
      if(csCount > 2) io(7).setName("cs3")
    }

    bundle
  }

  def asUartMaster(withInt: Boolean=false, withReset: Boolean=false) = {
    val bundle = Pmod12Uart(withInt, withReset)
    bundle.connect(io(1), io(2), io(0), io(3))

    if(withInt) {
      bundle.int.get := io(4).read
      io(4).write.assignDontCare()
      io(4).writeEnable:=False
    } else {
      bundle.gpio7.get <> io(4)
      bundle.gpio7.get.write.assignDontCare()
      bundle.gpio7.get.writeEnable.default(False)
    }

    if(withReset) {
      io(5).write := bundle.reset.get
      io(5).writeEnable := True
    } else {
      bundle.gpio8.get <> io(5)
      bundle.gpio8.get.write.assignDontCare()
      bundle.gpio8.get.writeEnable.default(False)
    }
  }
}

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
