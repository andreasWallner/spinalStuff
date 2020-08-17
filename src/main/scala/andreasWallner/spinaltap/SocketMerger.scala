package andreasWallner.spinaltap

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class SocketMerger(socketCnt: Int) extends Component {
  val io = new Bundle {
    val sockets = in Vec(Stream(Bits(16 bit)), 1)
    val all = master(Stream(Bits(16 bit)))
  }
  val neededWidth = java.lang.Integer.SIZE - java.lang.Integer.numberOfLeadingZeros(socketCnt)
  assert(neededWidth <= 8)
  val packetLen = Reg(UInt(8 bit)) init (0)
  val active = Reg(UInt(neededWidth bit))

  val fsm = new StateMachine {
    val waiting = new State with EntryPoint
    val header = new State
    val data = new State

    waiting
      .whenIsActive {
        
      }
  }
  when (packetLen === 0) {
    when(io.sockets(0).valid) {
      active := 0
    }
    when(io.all.valid) {
      active := io.all.payload(8 until 8 + neededWidth).asUInt
      packetLen := io.all.payload(0 to 7).asUInt
    }
  } otherwise {
    io.all.valid := active.muxList(False, for(idx <- io.sockets.indices) yield (idx, io.sockets(idx).valid))
    switch(active) {
      for(i <- 0 until socketCnt) is(i) { io.sockets(i).ready := io.all.ready }
    }
    when(io.all.ready && io.all.valid) {
      packetLen := packetLen - 1
    }
  }
  
}