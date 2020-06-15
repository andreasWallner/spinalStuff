package andreasWallner.test

import spinal.core._
import spinal.lib._
import spinal.lib.io.{Gpio, Apb3Gpio2}
import spinal.lib.bus.amba3.apb.{Apb3Config, Apb3Decoder}
import andreasWallner.ztex.{HsiInterface, FX3, BusMaster}
import andreasWallner.misc.Xorshift

case class IOControl() extends Component {
  val io = new Bundle {
    val fx3 = master(FX3())
    val led = out Bits(10 bit)
  }
  val hsi = HsiInterface()
  io.fx3 <> hsi.io.fx3
  hsi.io.tx.pktend := False
  hsi.io.tx.en := True
  hsi.io.tx.pktend_timeout := 100
  val bm = BusMaster()
  bm.io.data <> hsi.io.rx.data
  bm.io.resp <> hsi.io.tx.data

  val gpio = Apb3Gpio2(Gpio.Parameter(width = 10), Apb3Config(4, 32))
  io.led := gpio.io.gpio.write
  gpio.io.gpio.read := io.led

  val apbDecoder = Apb3Decoder(
    master = bm.io.apb3,
    slaves = List(
      gpio.io.bus -> (0x0000, 4 KiB)
    )
  )
}