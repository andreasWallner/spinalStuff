package andreasWallner.demos.icebreaker

import andreasWallner.demos.RgbLed
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

case class GPIOPmod12() extends PMOD12[GPIOPmod12] {
  val gpio = pins
  pins.foreach(pin => pin.writeEnable.default(False))
  pins.foreach(pin => pin.write.default(False))
}

case class iCEBreakerSnapOff() extends PMOD12[iCEBreakerSnapOff] {
  private val switchIdx = Seq(3, 6, 7)
  private val ledIdx = Seq(0, 1, 2, 4, 5)

  val sw2 = pins(6).read
  val sw3 = pins(3).read
  val sw4 = pins(7).read
  for(i <- switchIdx) {
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
  val pmod2: Option[P2] = Option(!useFifo generate master port Pmod2())
  pmod1a.setPrefix("P1A")
  pmod1b.setPrefix("P1B")
  pmod2.map(_.setPrefix("P2_"))

  val greenLed = out port Bool().setName("LEDG_N").default(True)
  val redLed = out port Bool().setName("LEDR_N").default(True)
  val userButton = in port Bool().setName("BTN_N")
  val rgbLed = out port RgbLed()
  rgbLed.r.setName("LED_RED_N").default(True)
  rgbLed.g.setName("LED_GRN_N").default(True)
  rgbLed.b.setName("LED_BLU_N").default(True)

  val uart = !useFifo generate (master port Uart(ctsGen = false, rtsGen = false))
  uart.txd.default(True).setName("TX")
  uart.rxd.setName("RX")

  val fifo = useFifo generate new Bundle {
    val data = master port TriState(Bits(8 bit))
    val rxf_n = in port Bool()
    val rxe_n = in port Bool()
    val rd_n = out port Bool()
    val wr_n = out port Bool()
  }
}
