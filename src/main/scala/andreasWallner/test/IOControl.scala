package andreasWallner.test

import spinal.core._
import spinal.lib._
import spinal.lib.io.{Gpio, Apb3Gpio2}
import spinal.lib.bus.amba3.apb.{Apb3Config, Apb3Decoder}
import andreasWallner.ztex.BusMaster
import andreasWallner.io.fx3._
import andreasWallner.io.pwm._
import andreasWallner.misc.Xorshift
import andreasWallner.spinaltap._

case class IOControl() extends Component {
  val io = new Bundle {
    val fx3 = master(SlaveFifo())
    val led = out Bits (10 bit)
    val pwmLed = out Bits(10 bit)
  }
  val sfm = SlaveFifoMaster()
  io.fx3 <> sfm.io.fx3
  sfm.io.tx.en := True
  sfm.io.tx.pktend_timeout := 100
  val bm = BusMaster()
  bm.io.data <> sfm.io.rx.data
  bm.io.resp <> sfm.io.tx.data
  sfm.io.tx.pktend := bm.io.pktend
  bm.io.pktend_done := sfm.io.tx.pktend_done

  val gpio = Apb3Gpio2(Gpio.Parameter(width = 10), Apb3Config(4, 32))
  io.led := gpio.io.gpio.write
  gpio.io.gpio.read := io.led
  val pwm = Apb3Pwm(
    Pwm.PeripheralParameters(
      Pwm.CoreParameters(counterWidth = 32, channelCnt = 10),
      dividerWidth = 32
    ),
    busConfig = Apb3Config(6, 32)
  )
  io.pwmLed := pwm.io.pwm.asBits

  val uart = UartModule(1)
  uart.io.clockDivider := U(20)
  uart.io.uart.rxd := uart.io.uart.txd
  bm.io.events <> uart.io.events

  //val spi = SpiMaster

  val apbDecoder = Apb3Decoder(
    master = bm.io.apb3,
    slaves = List(
      gpio.io.bus -> (0x0000, 64 Byte),
      pwm.io.bus -> (0x100, 64 Byte),
      uart.io.bus -> (0x200, 64 Byte)
    )
  )
}
