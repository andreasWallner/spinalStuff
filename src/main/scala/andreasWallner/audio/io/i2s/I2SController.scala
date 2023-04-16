package andreasWallner.audio.io.i2s

import spinal.core._
import spinal.lib._

// https://web.archive.org/web/20070102004400/http://www.nxp.com/acrobat_download/various/I2SBUS.pdf

case class I2SController(divider: Int, width: Int) extends Component {
  val io = new Bundle {
    val i2s = new Bundle {
      val ws = out port Bool().setAsReg init False
      val sck = out port Bool().setAsReg
    }
  }

  val ctr = Counter(0, divider - 1)
  ctr.increment()

  val bitCnt = Counter(0, width - 1)
  when(ctr.willOverflowIfInc) {
    io.i2s.sck := !io.i2s.sck

    when(io.i2s.sck) {
      bitCnt.increment()
      when(bitCnt.willOverflowIfInc) {
        io.i2s.ws := !io.i2s.ws
      }
    }
  }
}
