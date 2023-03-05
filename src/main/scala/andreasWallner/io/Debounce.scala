package andreasWallner.io

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Simple debounce that blocks changes for `stableTime` after each change */
object Debounce {
  def apply(b: Bool, stableCycles: CyclesCount): Bool = {
    val debounce = new Debounce(stableCycles)
    debounce.io.i := b
    debounce.io.o
  }
  def apply(b: Bool, stableTime: TimeNumber): Bool =
    apply(b, (stableTime * ClockDomain.current.frequency.getValue).toBigInt cycles)

  def apply(stableCycles: BigInt) = new Debounce(stableCycles cycles)
  def apply(stableTime: TimeNumber) =
    new Debounce((stableTime * ClockDomain.current.frequency.getValue).toBigInt cycles)
}

class Debounce(stableCycles: CyclesCount) extends Component {
  val io = new Bundle {
    val i = in port Bool()
    val o = out port Bool() setAsReg ()
  }
  val timeout = Timeout(stableCycles)
  when(timeout) {
    io.o := io.i
  }
  when(io.i.edge()) {
    timeout.clear()
  }
}
