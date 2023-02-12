package andreasWallner.misc

import andreasWallner.Utils.divCeil
import andreasWallner.yosys.YosysBenchmark
import spinal.core._

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
}
*/
object BenchmarkPriorityEncoder
    extends YosysBenchmark(
      ("plain 5 in", () => PriorityEncoder(5)),
      ("plain 12 in", () => PriorityEncoder(12)),
      ("plain 21 in", () => PriorityEncoder(21)),
      ("recursive 21/4", () => RecursivePriorityEncoder(21, 4)),
      ("recursive 21/2", () => RecursivePriorityEncoder(21, 2))
    )
