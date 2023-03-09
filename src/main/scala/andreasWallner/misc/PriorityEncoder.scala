package andreasWallner.misc

import andreasWallner.Utils.divCeil
import andreasWallner.yosys.YosysBenchmark
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PriorityEncoder(width: Int, encodeInvalid: Boolean = false) extends Component {
  val outputStates = if (!encodeInvalid) width else width + 1
  val io = new Bundle {
    val bits = in port Bits(width bit)
    val encoded = out port UInt(log2Up(outputStates) bit)
    val valid = out port Bool()
  }

  io.encoded.assignDontCare()
  switch(io.bits) {
    for (i <- 0 until width) {
      is(new MaskedLiteral(BigInt(1) << i, (BigInt(1) << (i + 1)) - 1, width)) {
        io.encoded := U(i)
      }
    }
  }
  io.valid := io.bits.orR
}

@deprecated("not fully tested, needs specific numbers")
case class RecursivePriorityEncoder(width: Int, muxWidth: Int) extends Component {
  val io = new Bundle {
    val bits = in port Bits(width bit)
    val encoded = out port UInt(log2Up(width) bit)
    val valid = out port Bool()
  }

  val (vs, es) = firstLayer(io.bits).unzip
  val (valid, encoded) = muxLayer(vs, es)
  io.valid := valid
  io.encoded := encoded

  private def subEncoder(b: Bits) = {
    // TODO decide on whether to instantiate a recursive encoder (and really recurse) or a normal one
    val pe = PriorityEncoder(b.getWidth)
    pe.io.bits := b
    (pe.io.valid, pe.io.encoded)
  }

  private def firstLayer(inputs: Bits) = {
    val inputWidth = divCeil(width, muxWidth)
    val resized = inputs.resize(inputWidth * muxWidth bit)
    val slices = resized.subdivideIn(muxWidth slices)
    for (slice <- slices) yield subEncoder(slice)
  }

  private def muxLayer(inputs: Seq[Bool], selectVals: Seq[UInt]) = {
    assert(inputs.size == selectVals.size)
    val width = inputs.size

    val (valid, encoded) = subEncoder(Cat(inputs))

    val muxed = UInt(selectVals.head.getWidth bit)
    muxed.assignDontCare()
    switch(encoded) {
      for (i <- 0 until width) is(i) { muxed := selectVals(i) }
    }

    (valid, (encoded ## muxed).asUInt)
  }
}

object PriorityBundle {
  def apply(prioWidth: Int, signalWidth: Int) = new PriorityBundle(prioWidth, signalWidth)
  def apply(any: Bool, prio: UInt, signals: Bits) = {
    val bundle = new PriorityBundle(prio.getWidth, signals.getWidth)
    bundle.any := any
    bundle.prio := prio
    bundle.signals := signals
    bundle
  }
}

class PriorityBundle(prioWidth: Int, signalWidth: Int) extends Bundle {
  val any = Bool()
  val prio = UInt(prioWidth bit)
  val signals = Bits(signalWidth bit)
}

case class PriorityMaskCell(prioWidth: Int, widthA: Int, widthB: Int) extends Component {
  val io = new Bundle {
    val a = in port PriorityBundle(prioWidth, widthA)
    val b = in port PriorityBundle(prioWidth, widthB)
    val result = out port PriorityBundle(prioWidth, widthA + widthB)
  }
  val aGtB = io.a.prio > io.b.prio
  val allowA = (aGtB & io.a.any) | !io.b.any
  val allowB = (!aGtB & io.b.any) | !io.a.any

  io.result.any := io.a.any | io.b.any
  io.result.prio := allowA ? io.a.prio | io.b.prio
  io.result.signals :=
    (allowB ? io.b.signals | B(0, widthB bit)) ##
      (allowA ? io.a.signals | B(0, widthA bit))
}

/**
  * Mask bits in io.signals so that io.masked is onehot, according to io.priorities
  *
  * The io.signals bit with the highest associated priority stays set, all others
  * are masked out.
  */
case class RecursivePriorityGate(prioWidth: Int, width: Int) extends Component {
  val io = new Bundle {
    val signals = in port Bits(width bit)
    val priorities = in port Vec(UInt(prioWidth bit), width)
    val result = out port PriorityBundle(prioWidth, width)
  }

  val inputs = Vec.tabulate(width) { i =>
    PriorityBundle(io.signals(i), io.priorities(i), io.signals(i, 1 bit))
  }
  io.result := inputs.reduceBalancedTree { (a, b) =>
    val cell = PriorityMaskCell(prioWidth, a.signals.getWidth, b.signals.getWidth)
    cell.io.a := a
    cell.io.b := b
    cell.io.result
  }
}

/**
 * Mask bits in io.signals so that io.masked is onehot, according to io.priorities
 *
 * The io.signals bit with the highest associated priority stays set, all others
 * are masked out.
 * Warning: the assigned priorities must be unique
 */
case class OneHotPriorityGate(prioWidth: Int, width: Int) extends Component {
  val io = new Bundle {
    val signals = in port Bits(width bit)
    val priorities = in port Vec(UInt(prioWidth bit), width)
    val masked = out port Bits(width bit)
  }
  val onehot = Vec.tabulate(width) { i =>
    val oh = (B"1" << io.priorities(i)).take(width)
    oh & B(io.signals(i), oh.getWidth)
  }
  val all = onehot.reduce(_ | _)
  val highestOh = OHMasking.last(all)

  onehot.zipWithIndex.foreach(x => {
    val (oh, idx) = x
    io.masked(idx) := (oh & highestOh).orR
  })
}

/**
 * Mask bits in io.signals so that io.masked is onehot, according to io.priorities
 *
 * The io.signals bit with the highest associated priority stays set, all others
 * are masked out.
 * Warning: the assigned priorities must be unique
 */
case class EqualityPriorityGate(prioWidth: Int, width: Int, allowNonUniquePriorities: Boolean = false) extends Component {
  val io = new Bundle {
    val signals = in port Bits(width bit)
    val priorities = in port Vec(UInt(prioWidth bit), width)
    val masked = out port Bits(width bit)
  }
  val maskedPrio = Vec.tabulate(width) { i => io.signals(i) ? io.priorities(i) | U(0) }
  val maxPrio = maskedPrio.reduceBalancedTree((a, b) => (a > b) ? a | b)
  val masked = B(0, width bit)
  (0 until width).foreach(i =>
    masked(i) := (io.priorities(i) === maxPrio) && io.signals(i)
  )
  io.masked := (if(allowNonUniquePriorities) OHMasking.last(masked) else masked)
}

// TODO encode bit-by-bit on the fly in tree with binary output

/*
case class DynamicPriorityEncoder(width: Int) extends Component {
  val io = new Bundle {
    val bits = in port Bits(width bit)
    val priorities = in port Vec(UInt(log2Up(width) bit), width)
    val encoded = out port UInt(log2Up(width) bit)
    val valid = out port Bool()
  }

  val sortXs = Vec.tabulate(width) { i =>
    val sortX = Bits(width bit)
    sortX.clearAll()
    switch(io.priorities(i)) {
      for (x <- 0 until width) is(i) {
        sortX(x) := io.bits(i)
      }
    }
    sortX
  }

  val sorted = sortXs.orR

  val pe = PriorityEncoder(width).io
  pe.bits := sorted
  io.valid := pe.valid
  io.encoded := pe.encoded
}*/

object BenchmarkPriorityEncoder
    extends YosysBenchmark(
      ("plain 5 in", () => PriorityEncoder(5)),
      ("plain 12 in", () => PriorityEncoder(12)),
      ("plain 21 in", () => PriorityEncoder(21)),
      ("recursive 21/4", () => RecursivePriorityEncoder(21, 4)),
      ("recursive 21/2", () => RecursivePriorityEncoder(21, 2))
    )
object BenchmarkPriorityGate
    extends YosysBenchmark(
      Seq(4, 7, 10, 13)
        .flatMap { w =>
          Seq(
            (f"$w%2d recursive gate", () => RecursivePriorityGate(log2Up(w), w)),
            (f"$w%2d onehot gate", () => OneHotPriorityGate(log2Up(w), w)),
            (f"$w%2d equality gate", () => EqualityPriorityGate(log2Up(w), w)),
            (f"$w%2d equality gate nq", () => EqualityPriorityGate(log2Up(w), w, true))
          )
        }
        .sortWith(_._1 < _._1): _*
    )
