package andreasWallner.audio.io.i2s

import spinal.core._
import spinal.lib._

// https://web.archive.org/web/20070102004400/http://www.nxp.com/acrobat_download/various/I2SBUS.pdf
// TODO defined starting value, defined underflow value

case class I2STransmitter(width: Int) extends Component {
  val io = new Bundle {
    val i2s = new Bundle {
      val sd = out Bool ()
      val ws = in Bool ()
      val sck = in Bool ()
    }
    val audio = new Bundle {
      val left = slave(Stream(UInt(width bit)))
      val right = slave(Stream(UInt(width bit)))
    }
  }

  io.audio.left.ready := False
  io.audio.right.ready := False

  val buffer = Reg(UInt(width bit))
  io.i2s.sd := buffer.msb

  val sck_rise = io.i2s.sck.rise()
  val sck_fall = io.i2s.sck.fall()

  val ws_sync = RegNextWhen(io.i2s.ws, sck_rise)
  val ws_sync_d_f = RegNextWhen(ws_sync, sck_fall)

  when(io.i2s.sck.fall()) {
    buffer := buffer |<< 1
  }
  when(!ws_sync && ws_sync_d_f && sck_fall) {
    buffer := io.audio.left.payload
    io.audio.left.ready := True
  } elsewhen (ws_sync && !ws_sync_d_f && sck_fall) {
    buffer := io.audio.right.payload
    io.audio.right.ready := True
  }
}
