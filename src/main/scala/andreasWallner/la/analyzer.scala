package andreasWallner.la

import andreasWallner.Utils.gcd
import spinal.core._
import spinal.lib._

import scala.collection.mutable

case class AnalyzerGenerics(
    dataWidth: Int,
    externalTriggerCnt: Int,
    rleCounterWidth: Int
)

object TriggerMode extends SpinalEnum {
  val disabled, rising, falling, high, low = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    disabled -> 0,
    rising -> 7, // 0b111
    falling -> 3, // 0b011
    high -> 5, // 0b101
    low -> 1 // 0b001
  )
}

case class SimpleTriggerUnit(width: Int) extends Component {
  val io = new Bundle {
    val data = in Bits (width bits)
    val mode = in Vec (TriggerMode(), width)
    val trigger = out Bool ()
  }
  val active = Bits(width bits)
  for (i <- 0 to width) {
    active(i) := io
      .mode(i)
      .mux(
        TriggerMode.rising -> io.data(i).rise(),
        TriggerMode.falling -> io.data(i).fall(),
        TriggerMode.high -> io.data(i),
        TriggerMode.low -> !io.data(i),
        default -> True
      )
  }
  io.trigger := active.orR
}

case class CompressedData(tw: Int, dw: Int) extends Bundle {
  val time = UInt(tw bits)
  val data = Bits(dw bits)
}

case class RLECompressor(g: AnalyzerGenerics) extends Component {
  val io = new Bundle {
    val data = in Bits (g.dataWidth bits)
    val compressed = master(
      Flow(CompressedData(g.rleCounterWidth, g.dataWidth))
    )
    val run = in Bool ()
  }
  val delayData = RegNext(io.data)
  val counter = Reg(UInt(g.rleCounterWidth bits))
  when(io.run) {
    counter := counter - 1
  } otherwise {
    counter := 0
  }

  io.compressed.valid := (io.run && (counter === 0 || (io.data ^ delayData).orR))
  io.compressed.payload.data := io.data
  io.compressed.payload.time := counter
}

object MemoryFormatter {
  def makeMultipleOf(v: Int, multiple: Int): Int = {
    ((v + multiple - 1) / multiple) * multiple
  }

  def rotate[A](seq: Seq[A], n: Int): Seq[A] = {
    val nn =
      if (n > 0) n % seq.length else seq.length - (n.abs % seq.length)
    seq.view.takeRight(nn) ++ seq.view.dropRight(nn)
  }

  // to only require a +1 upcounter, we count the insert positions 0, 1, 2
  // but actually mean the first, second, ... insert offset (if we have 3
  // input blocks that would be 0, 3, 6
  // generate decoder from insert position to vector of enables
  // if inputBlocks and outputBlocks share a common divider, not all
  // indices might actually be needed, deal with this by adding until we
  // completed a full cycle
  def enableDecoder(
      outputCnt: Int,
      bufferCnt: Int,
      inputBlocks: Int
  ): Seq[(Any, Bits)] = {
    val v = mutable.ArrayBuffer[(Any, Bits)]()
    var idx = 0
    var insertOffset = 0
    do {
      assert(v.count(_ => true) < outputCnt)
      val en = for (i <- insertOffset until insertOffset + inputBlocks) yield {
        i % bufferCnt -> true
      }
      v.append((idx, B(bufferCnt bits, default -> false, en: _*)))
      idx += 1
      insertOffset = (insertOffset + inputBlocks) % outputCnt
    } while (insertOffset != 0)

    v
  }

  // generate mux mappings depending on the current inputOffset
  // for a single mux so that data can be stored in the correct mux
  def muxIndices(muxCnt: Int, mux: Int, offset: Int): Seq[Int] = {
    // note: muxCnt is also the number of inputs per mux
    for (input <- 0 until muxCnt) yield {
      val initial = muxCnt - 1 downto 0
      //println(f"""$input / $mux: ${initial mkString( " ")} >> ${(offset*input)%muxCnt} = ${rotate(initial, (offset * input) % muxCnt) mkString(" ")}""")
      rotate(initial, (offset * input) % muxCnt)(mux)
    }
  }

  def apply[T1 <: Data, T2 <: Data](
      input: Stream[T1],
      output: Stream[T2],
      pieceWidth: Int = 8,
      noDelay: Boolean = true
  ) {
    val inputWidth = widthOf(input.payload)
    val paddedInputWidth = makeMultipleOf(inputWidth, pieceWidth)
    val outputWidth = widthOf(output.payload)
    assert(paddedInputWidth < widthOf(output.payload))
    assert(widthOf(output.payload) % pieceWidth == 0)

    val inputBlocks = paddedInputWidth / pieceWidth
    val outputBlocks = outputWidth / pieceWidth
    val bufferBlocks = inputBlocks + outputBlocks - gcd(
      inputBlocks,
      outputBlocks
    )
    val lastOutputElement = inputBlocks - 1
    val secondLoopOffset = inputBlocks - (outputBlocks % inputBlocks)

    val buffers =
      Vec(Reg(Bits(pieceWidth bits)), bufferBlocks).setName("buffers")
    val slicedInput =
      input.payload.asBits.resize(paddedInputWidth).subdivideIn(pieceWidth bits)

    val inputOffset = Counter(0 until inputBlocks)
    inputOffset.value.setName("inputOffset")
    val muxes = for (ii <- 0 until inputBlocks) yield {
      val indices = 0 until inputBlocks
      val results = muxIndices(inputBlocks, ii, secondLoopOffset)
      inputOffset.value
        .muxList(
          (indices zip results).map({
            case (i: Int, r: Int) => (i, slicedInput(r))
          }) ++ (if (outputWidth % inputWidth != 0)
                   List((spinal.core.default, slicedInput(0)))
                 else List())
        )
        .setName(f"mux$ii")
    }

    val enableEncodings = enableDecoder(outputBlocks, bufferBlocks, inputBlocks)
    val storeOffset = Counter(0 until enableEncodings.count(_ => true))
    storeOffset.value.setName("storeOffset")
    val enables =
      storeOffset
        .muxList(enableDecoder(outputBlocks, bufferBlocks, inputBlocks))
        .reversed
        .setName("enables")

    val input_fire = input.fire

    val bufferValid = Reg(Bool()) init False
    bufferValid
      .setWhen(enables(lastOutputElement) && input_fire)
      .clearWhen(output.fire)
      .setName("bufferValid")
    when(input_fire) {
      storeOffset.increment()
      when(enables(lastOutputElement)) { inputOffset.increment() }
    }

    for (ii <- bufferBlocks - 1 downto 0) {
      when(enables(ii) && input_fire) {
        buffers(ii) := muxes((bufferBlocks - 1 - ii) % muxes.length)
      }
    }
    val trueBufferBlocks = bufferBlocks - outputBlocks
    val delayedEnables = RegNextWhen(
      enables(trueBufferBlocks - 1 downto 0),
      input_fire
    ) init 0
    delayedEnables.setName("delayedEnables")
    for (ii <- 0 until trueBufferBlocks) {
      when(delayedEnables(ii) && input_fire) {
        buffers(outputBlocks + ii) := buffers(ii)
      }
    }

    val outputs = if (noDelay) {
      (for (ii <- bufferBlocks - 1 downto trueBufferBlocks) yield {
        val muxedFromBuffer = ii >= bufferBlocks - trueBufferBlocks // TODO only need if input/output size relation fits (output < 2*input?)
        val muxedFromInput = ii >= trueBufferBlocks && ii < trueBufferBlocks + inputBlocks
        val bufferIdx = ii - outputBlocks
        val muxIdx = (bufferBlocks - 1 - ii) % muxes.length
        (muxedFromBuffer, muxedFromInput) match {
          case (false, false) => buffers(ii)
          case (true, false) =>
            (delayedEnables(bufferIdx) && !bufferValid) ? buffers(bufferIdx) | buffers(ii)
          case (false, true) =>
            (enables(ii) && !bufferValid) ? muxes(muxIdx) | buffers(ii)
          case (true, true) =>
            (enables(ii) && !bufferValid) ? muxes(muxIdx) | (
              (delayedEnables(bufferIdx) && !bufferValid) ? buffers(bufferIdx) | buffers(ii))
        }
      }).reverse
    } else buffers.takeRight(outputBlocks)

    output.payload.assignFromBits(Cat(outputs))
    input.ready := !bufferValid
    output.valid := bufferValid | (if (noDelay)
                                     enables(lastOutputElement) && input_fire && !bufferValid
                                   else False)
  }
}

case class Analyzer(g: AnalyzerGenerics) extends Component {
  val io = new Bundle {
    val data = in Bits (g.dataWidth bits)
    val externalTrigger = in Bits (g.externalTriggerCnt bits)
    val triggerMode = in Vec (TriggerMode(), g.dataWidth)
  }

  val triggerUnit = SimpleTriggerUnit(g.dataWidth)
  val compressor = RLECompressor(g)

  triggerUnit.io.data := io.data
  triggerUnit.io.mode := io.triggerMode
  val doTrigger = triggerUnit.io.trigger || io.externalTrigger.orR
}
