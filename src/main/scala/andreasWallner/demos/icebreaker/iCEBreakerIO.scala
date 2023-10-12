package andreasWallner.demos.icebreaker

import andreasWallner.io.ftdi.AsyncFifo
import andreasWallner.io.RgbLed
import spinal.core._
import spinal.lib._
import spinal.lib.com.uart.Uart
import spinal.lib.io.TriState

import scala.reflect.io.File
import scala.language.postfixOps

abstract class PMOD12[T <: PMOD12[_]]() extends Bundle with IMasterSlave {
  protected val pins = Vec(TriState(Bool()), 8)
  //pins.foreach(pin => pin.writeEnable.default(False))
  //pins.foreach(pin => pin.write.default(False))

  def setPrefix(prefix: String): T = {
    for ((pin, idx) <- pins.zipWithIndex) {
      pin.setName(if (idx <= 3) f"$prefix${idx + 1}" else f"$prefix${idx + 3}")
    }
    this.asInstanceOf[T]
  }

  override def asMaster(): Unit = pins.foreach(pin => pin.setAsMaster())

  def raw() = pins
}

abstract class PMOD6[T <: PMOD6[_]]() extends Bundle with IMasterSlave {
  protected val pins = Vec(TriState(Bool()), 4)

  def setPrefix(prefix: String): T = {
    for ((pin, idx) <- pins.zipWithIndex) {
      pin.setName(f"$prefix${idx + 1}")
    }
    this.asInstanceOf[T]
  }

  override def asMaster(): Unit = pins.foreach(pin => pin.setAsMaster())
  def raw() = pins
}

case class SplitPmod12[T1 <: PMOD6[_], T2 <: PMOD6[_]](Upper: HardType[T1], Lower: HardType[T2])
    extends PMOD12[SplitPmod12[T1, T2]] {
  val upper = Upper()
  val lower = Lower()
  def conn(m: TriState[Bool], s: TriState[Bool]) = {
    m.write := s.write
    m.writeEnable := s.writeEnable
    s.read := m.read
  }
  // TODO we can't use <> here
  for (i <- 0 until 3)
    conn(pins(i), upper.raw()(i))
  for (i <- 0 until 3)
    conn(pins(i+4), lower.raw()(i))

  override def asMaster(): Unit = {
    master(upper, lower)
  }
}

case class GPIOPmod12() extends PMOD12[GPIOPmod12] {
  val gpio = pins
  pins.foreach(pin => pin.writeEnable.default(False))
  pins.foreach(pin => pin.write.default(False))
}

case class GPIOPmod6() extends PMOD6[GPIOPmod6] {
  val gpio = pins
  pins.foreach(pin => pin.writeEnable.default(False))
  pins.foreach(pin => pin.write.default(False))
}

case class UARTPmod6(withFlowControl: Boolean=false) extends PMOD6[GPIOPmod6] {
  val uart = new Uart(ctsGen = withFlowControl, rtsGen = withFlowControl)
  val gpio1 = if (withFlowControl) None else Some(TriState(Bool()))
  val gpio4 = if (withFlowControl) None else Some(TriState(Bool()))

  pins(0).write := uart.txd
  pins(0).writeEnable := True
  uart.rxd := pins(1).read
  pins(1).writeEnable := False
  pins(1).write.assignDontCare()

  if(withFlowControl) {
    uart.cts := pins(2).read
    pins(2).writeEnable := True
    pins(2).write.assignDontCare()

    pins(3).write := uart.rts
    pins(3).writeEnable := True
  } else {
    def conn(m: TriState[Bool], s: TriState[Bool]) = {
      m.write := s.write
      m.writeEnable := s.writeEnable
      s.read := m.read
    }
    conn(gpio1.get, pins(2))
    conn(gpio4.get, pins(3))
    gpio1.get.write.default(False)
    gpio1.get.write.assignDontCare()
    gpio4.get.write.default(False)
    gpio4.get.write.assignDontCare()
  }
}

case class iCEBreakerSnapOff() extends PMOD12[iCEBreakerSnapOff] {
  private val switchIdx = Seq(3, 6, 7)
  private val ledIdx = Seq(0, 1, 2, 4, 5)

  val sw2 = pins(6).read
  val sw3 = pins(3).read
  val sw4 = pins(7).read
  for (i <- switchIdx) {
    pins(i).write.default(False)
    pins(i).writeEnable.default(False)
  }

  val led_north = pins(5).write
  val led_south = pins(2).write
  val led_east = pins(1).write
  val led_west = pins(0).write
  val led_center = pins(4).write
  for (i <- ledIdx) {
    pins(i).write.default(False)
    pins(i).writeEnable.default(True)
  }

  def buttons = Seq(sw2, sw3, sw4)
  def leds = Seq(led_center, led_west, led_north, led_east, led_south)
}

object iCEBreakerIO {
  def pcfFile = File(".").toCanonical + "extras/iCEBreaker.pcf"
}

class iCEBreakerIO[P1A <: PMOD12[_], P1B <: PMOD12[_], P2 <: PMOD12[_]](
    Pmod1a: HardType[P1A],
    Pmod1b: HardType[P1B],
    Pmod2: HardType[P2],
    useFifo: Boolean = false
) extends Bundle {
  val pmod1a: P1A = master port Pmod1a()
  val pmod1b: P1B = master port Pmod1b()
  val pmod2: Option[P2] = Option(!useFifo generate { master port Pmod2() })
  pmod1a.setPrefix("P1A")
  pmod1b.setPrefix("P1B")
  pmod2.map(_.setPrefix("P2_"))

  val greenLed = !useFifo generate (out port Bool().setName("LEDG_N").default(True))
  val redLed = !useFifo generate (out port Bool().setName("LEDR_N").default(True))
  val userButton = in port Bool().setName("BTN_N")
  val rgbLed = out port RgbLed()
  rgbLed.r.setName("LED_RED_N").default(True)
  rgbLed.g.setName("LED_GRN_N").default(True)
  rgbLed.b.setName("LED_BLU_N").default(True)

  val uart = !useFifo generate (master port Uart(ctsGen = false, rtsGen = false))
  if (!useFifo) {
    uart.txd.default(True).setName("TX")
    uart.rxd.setName("RX")
  }

  val fifo = useFifo generate (master port AsyncFifo())
}
