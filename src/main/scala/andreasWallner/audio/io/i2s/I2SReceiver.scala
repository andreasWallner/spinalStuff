package andreasWallner.audio.io.i2s

import spinal.core._
import spinal.lib._

// https://web.archive.org/web/20070102004400/http://www.nxp.com/acrobat_download/various/I2SBUS.pdf

case class I2SReceiver(width: Int) extends Component {
  val io = new Bundle {
    val i2s = new Bundle {
      val sd = in Bool ()
      val ws = in Bool ()
      val sck = in Bool ()
    }
    val audio = new Bundle {
      val left = master(Flow(UInt(width bit)))
      val right = master(Flow(UInt(width bit)))
    }
  }

  val ws_sync = RegNextWhen(io.i2s.ws, io.i2s.sck.rise())

  val buffer = Reg(UInt(width bit))
  val bitcnt = Reg(UInt(log2Up(width + 1) bit))
  val bitcnt_max = U(bitcnt.range -> true)
  bitcnt init bitcnt_max

  val left_next = ws_sync.rise()
  val right_next = ws_sync.fall()
  io.audio.left.payload := buffer
  io.audio.right.payload := buffer
  io.audio.left.valid := left_next.rise()
  io.audio.right.valid := right_next.rise()

  when(left_next || right_next) {
    buffer := 0
    bitcnt := width - 1
  }

  when(io.i2s.sck.rise() && (bitcnt =/= bitcnt_max)) {
    buffer(bitcnt(0 until log2Up(width))) := io.i2s.sd
    bitcnt := bitcnt - 1
  }
}
