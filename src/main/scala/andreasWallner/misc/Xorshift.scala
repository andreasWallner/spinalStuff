package andreasWallner.misc

import spinal.core._
import spinal.lib._

case class Xorshift() extends Component {
  val io = new Bundle {
    val run = in Bool()
    val data = master Stream(Bits(16 bit))
  }

  val state = Reg(Bits(16 bit)) init(1)
  io.data.payload := state
  io.data.valid := io.run
  val temp1 = Bits(16 bit)
  val temp2 = Bits(16 bit)
  val temp3 = Bits(16 bit)
  temp1 := state ^ (state |<< 7)
  temp2 := temp1 ^ (temp1 |>> 9)
  temp3 := temp2 ^ (temp2 |<< 8)
  when(!io.run) {
    state := 1
  } elsewhen(io.data.valid && io.data.ready) {
    state := temp3
  }
}