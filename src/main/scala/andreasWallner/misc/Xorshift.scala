package andreasWallner.misc

import spinal.core._
import spinal.lib._

case class XorShiftConfig(
    width: Int,
    hasRun: Boolean = false,
    hasSetSeed: Boolean = false
)

case class Xorshift(config: XorShiftConfig = XorShiftConfig(16))
    extends Component {
  val io = new Bundle {
    val run = config.hasRun generate { in Bool () }
    val seed = config.hasSetSeed generate { in Bits (config.width bit) }
    val setSeed = config.hasSetSeed generate { in Bool () }
    val data = master Stream Bits(config.width bit)
  }

  assert(List(16, 32, 64).contains(config.width))
  val shifts = config.width match {
    case 16 => List(7, 9, 8)
    case 32 => List(13, 17, 5)
    case 64 => List(13, 7, 17)
  }

  val state = Reg(Bits(config.width bit)) init 1
  io.data.payload := state
  io.data.valid := (if (config.hasRun) io.run else True) && (if (config.hasSetSeed)
                                                               !io.setSeed
                                                             else True)
  val temp1 = state ^ (state |<< shifts(0))
  val temp2 = temp1 ^ (temp1 |>> shifts(1))
  val temp3 = temp2 ^ (temp2 |<< shifts(2))
  when(io.data.fire) {
    state := temp3
  }
  if (config.hasSetSeed)
    when(io.setSeed) {
      state := io.seed
    }
}
