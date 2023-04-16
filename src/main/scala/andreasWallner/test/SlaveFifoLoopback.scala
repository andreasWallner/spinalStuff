package andreasWallner.test

import spinal.core._
import spinal.lib._
import andreasWallner.io.fx3._
import andreasWallner.misc.{XorShiftConfig, Xorshift}

import scala.language.postfixOps

case class SlaveFifoLoopback() extends Component {
  val io = new Bundle {
    val fx3 = master port SlaveFifo()
    val activity = out port Bool()
    val mode = in port Bits(4 bit)

    val toggle1Hz = out port Bool().setAsReg
  }

  val sfmTxEn = RegInit(False)
  val sfm = SlaveFifoMaster()
  io.fx3 <> sfm.io.fx3
  sfm.io.tx.pktend := False
  sfm.io.tx.pktend_timeout := 10000
  sfm.io.tx.en := sfmTxEn

  val xorshift = Xorshift(XorShiftConfig(16, hasRun=true))
  xorshift.io.run := io.mode(0)
  val fifo = StreamFifo(
    dataType = Bits(16 bit),
    depth = 1024
  )
  fifo.io.push << sfm.io.rx.data
  val source = StreamArbiterFactory.lowerFirst.noLock.onArgs(xorshift.io.data, fifo.io.pop)
  source >> sfm.io.tx.data

  val rxTimeout = Timeout(100)
  when((sfm.io.rx.data.ready && sfm.io.rx.data.valid) || sfmTxEn || io.mode(0)) { rxTimeout.clear() }
  when(fifo.io.occupancy === 1022 || rxTimeout || io.mode(0) ) {
    sfmTxEn := True
  } elsewhen(fifo.io.occupancy === 0) {
    sfmTxEn := False
  }
  io.activity := fifo.io.occupancy =/= 0

  val timeout1Hz = Timeout(1 sec)
  when(timeout1Hz) {
    io.toggle1Hz := !io.toggle1Hz
    timeout1Hz.clear()
  }
}
