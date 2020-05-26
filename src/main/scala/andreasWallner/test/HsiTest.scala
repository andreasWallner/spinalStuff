package andreasWallner.test

import spinal.core._
import spinal.lib._
import andreasWallner.ztex.{HsiInterface, FX3}
import andreasWallner.misc.Xorshift

case class HsiLoopbackTest() extends Component {
  val io = new Bundle {
    val fx3 = master(FX3())
    val activity = out Bool
    val mode = in Bits(4 bit)

    val toggle1Hz = out(Reg(Bool))
  }

  val hsiTxEn = RegInit(False)
  val hsi = HsiInterface()
  io.fx3 <> hsi.io.fx3
  hsi.io.tx.pktend := False
  hsi.io.tx.pktend_timeout := 10000
  hsi.io.tx.en := hsiTxEn
  
  val xorshift = Xorshift()
  xorshift.io.run := io.mode(0)
  val fifo = StreamFifo(
    dataType = Bits(16 bit),
    depth = 1024
  )
  fifo.io.push << hsi.io.rx.data
  val source = StreamArbiterFactory.lowerFirst.noLock.onArgs(xorshift.io.data, fifo.io.pop)
  source >> hsi.io.tx.data
  
  val rxTimeout = Timeout(100)
  when((hsi.io.rx.data.ready && hsi.io.rx.data.valid) || hsiTxEn || io.mode(0)) { rxTimeout.clear() }
  when(fifo.io.occupancy === 1022 || rxTimeout || io.mode(0) ) {
    hsiTxEn := True
  } elsewhen(fifo.io.occupancy === 0) {
    hsiTxEn := False
  }
  io.activity := fifo.io.occupancy =/= 0

  val timeout1Hz = Timeout(1 sec)
  when(timeout1Hz) {
    io.toggle1Hz := !io.toggle1Hz
    timeout1Hz.clear()
  }
}