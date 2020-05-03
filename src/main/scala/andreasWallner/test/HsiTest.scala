package andreasWallner.test

import spinal.core._
import spinal.lib._
import andreasWallner.ztex.{HsiInterface, FX3};

case class HsiLoopbackTest() extends Component {
  val io = new Bundle {
    val fx3 = master(FX3())
  }

  val hsiTxEn = RegInit(False)
  val hsi = HsiInterface()
  io.fx3 <> hsi.io.fx3
  hsi.io.tx.pktend := False
  hsi.io.tx.pktend_timeout := 0
  hsi.io.tx.en := hsiTxEn
  
  val fifo = StreamFifo(
    dataType = Bits(16 bit),
    depth = 1024
  )
  fifo.io.push << hsi.io.rx.data
  fifo.io.pop >> hsi.io.tx.data
  
  val rxTimeout = Timeout(100)
  when((hsi.io.rx.data.ready && hsi.io.rx.data.valid) || hsiTxEn) { rxTimeout.clear() }
  when(fifo.io.occupancy === 1022 || rxTimeout ) {
    hsiTxEn := True
  } elsewhen(fifo.io.occupancy === 0) {
    hsiTxEn := False
  }
}