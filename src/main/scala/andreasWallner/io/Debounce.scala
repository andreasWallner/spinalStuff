package andreasWallner.io

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Simple debounce that blocks changes for `stableTime` after each change */
object Debounce {
  def apply(
      b: Bool,
      stableCycles: CyclesCount,
      syncCycles: Option[Int] = None,
      syncInitial: Option[Bool] = None
  ): Bool = {
    val debounce = this(1, stableCycles)
    val input = (syncCycles, syncInitial) match {
      case (Some(depth), None)          => BufferCC(b, depth)
      case (Some(depth), Some(initial)) => BufferCC(b, initial, depth)
      case (None, _)                    => b
    }
    debounce.io.i(0) := input
    debounce.io.o(0)
  }
  def apply(
      b: Bool,
      stableTime: TimeNumber,
      syncCycles: Option[Int] = None,
      syncInitial: Option[Bool] = None
  ): Bool = {
    val debounce = this(1, stableTime)
    val input = (syncCycles, syncInitial) match {
      case (Some(depth), None)          => BufferCC(b, depth)
      case (Some(depth), Some(initial)) => BufferCC(b, initial, depth)
      case (None, _)                    => b
    }
    debounce.io.i(0) := input
    debounce.io.o(0)
  }

  def apply(b: Bits, stableCycles: CyclesCount, syncCycles: Option[Int] = None, syncInitial: Option[Bits] = None): Bits = {
    val debounce = this(widthOf(b), stableCycles)
      val input = (syncCycles, syncInitial) match {
      case (Some(depth), None)          => BufferCC(b, depth)
      case (Some(depth), Some(initial)) => BufferCC(b, initial, depth)
      case (None, _)                    => b
    }
    debounce.io.i := input
    debounce.io.o
  }
  def apply(b: Bits, stableTime: TimeNumber, syncCycles: Option[Int] = None, syncInitial: Option[Bits] = None): Bits = {
    val debounce = this(widthOf(b), stableTime)
    val input = (syncCycles, syncInitial) match {
    case (Some(depth), None)          => BufferCC(b, depth)
    case (Some(depth), Some(initial)) => BufferCC(b, initial, depth)
    case (None, _)                    => b
  }
    debounce.io.i := input
    debounce.io.o
  }

  def apply(width: Int, stableCycles: CyclesCount) = TimeoutDebounce(width, stableCycles)
  def apply(width: Int, stableTime: TimeNumber) =
    TimeoutDebounce(width, (stableTime * ClockDomain.current.frequency.getValue).toBigInt cycles)
}

case class TimeoutDebounce(width: Int, stableCycles: CyclesCount) extends Component {
  val io = new Bundle {
    val i = in port Bits(width bits)
    val o = out port Bits(width bits).setAsReg()
  }
  for (i <- 0 until width) {
    val timeout = Timeout(stableCycles)
    when(timeout) {
      io.o(i) := io.i(i)
    }
    when(io.i(i).edge()) {
      timeout.clear()
    }
  }
}

class UndersampleDebounce(width: BitCount, undersampleFactor: CyclesCount) extends Component {
  val io = new Bundle {
    val i = in port Bits(width)
    val o = out port Bits(width)
  }

  val sampling = new SlowArea(undersampleFactor) {
    val sampled = RegNext(io.i)
    io.o := sampled
  }
}

class UndersampleMajorityDebounce(width: BitCount, undersampleFactor: CyclesCount, votes: Int = 1)
    extends Component {
  assert(votes > 0, "can't do 0 votes for debouncing")
  val io = new Bundle {
    val i = in port Bits(width)
    val o = out port Bits(width)
  }

  val sampling = new SlowArea(undersampleFactor) {
    val sampled = History(io.i, votes)
    if (votes == 1) {
      io.o := sampled
    } else {}
  }
}
