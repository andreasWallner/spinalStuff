package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import scala.math.log

case class SocketArbiter(socketCnt : Int) extends Component {
  val io = new Bundle {
    val all = slave(Stream(Bits(16 bit)))
    val sockets = Vec(master(Stream(Bits(16 bit))), socketCnt)
  }
  val neededWidth = java.lang.Integer.SIZE - java.lang.Integer.numberOfLeadingZeros(socketCnt)
  assert(neededWidth <= 8)
  val active = Reg(UInt(neededWidth bit))
  val packetLen = Reg(UInt(8 bit)) init (0)

  io.all.ready := False
  for(socket <- io.sockets) {
    socket.valid := False
    socket.payload := io.all.payload
  }
  
  when(packetLen === 0) {
    io.all.ready := True
    when(io.all.valid) {
      active := io.all.payload(8 until 8 + neededWidth).asUInt
      packetLen := io.all.payload(0 to 7).asUInt
    }
  } otherwise {
    io.all.ready := active.muxList(False, for(idx <- io.sockets.indices) yield (idx, io.sockets(idx).ready))
    switch(active) {
      for(i <- 0 until socketCnt) is(i) { io.sockets(i).valid := io.all.valid }
    }
    when(io.all.ready && io.all.valid) {
      packetLen := packetLen - 1
    }
  }
}