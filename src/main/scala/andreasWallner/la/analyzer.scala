package andreasWallner.la

import andreasWallner.Utils.gcd
import andreasWallner.zynq.Axi3Dma
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4WriteOnly}

import scala.collection.mutable
import scala.collection.Seq
import scala.language.postfixOps
import andreasWallner.rtlutils._

case class AnalyzerGenerics(
    dataWidth: Int,
    internalWidth: Int,
    externalTriggerCnt: Int = 0
) {
  assert(
    dataWidth < internalWidth,
    "dataWidth must be smaller than internalWidth"
  )

  def compressedDataWidth = internalWidth - 1
}

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
  for (i <- 0 until width) {
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
  io.trigger := active.andR
}

case class CompressedData(width: Int) extends Bundle {
  val isData = Bool()
  val dataOrTime = Bits(width bits)
}

case class RLECompressor(g: AnalyzerGenerics) extends Component {
  val io = new Bundle {
    val data = in Bits (g.dataWidth bits)
    val compressed = master(Flow(CompressedData(g.compressedDataWidth)))
    val run = in Bool ()
  }
  val dataChange = io.data =/= RegNext(io.data)
  val skidBuffer = new Area {
    val hold = Bool()

    val delayedData = RegNext(io.data)
    val delayedChange = RegNext(dataChange)

    val choice = RegNext(hold) || (delayedChange && RegNext(delayedChange)) || io.run
      .rise()
    val data = choice ? delayedData | io.data
    val change = choice ? delayedChange | dataChange
  }
  val counter = Reg(UInt(g.compressedDataWidth bits))
  val maxCount = counter === counter.maxValue
  val sendCount = (dataChange.rise() || maxCount) && !io.run.rise()

  val send = maxCount || dataChange.rise() || skidBuffer.change || io.run.rise()
  skidBuffer.hold := maxCount || dataChange.rise()

  io.compressed.payload.isData := !sendCount
  io.compressed.payload.dataOrTime := sendCount ? counter.asBits | skidBuffer.data.resized
  io.compressed.valid := send && io.run

  when(io.run && !send) {
    counter := counter + 1
  } otherwise {
    counter := 0
  }
}

@deprecated("untested")
case class LEB128Counter(width: Int, chunkBits: Int = 8) extends Bundle {
  val validBits = (width + chunkBits - 1) / chunkBits
  val counterBits = width - validBits

  val io = new Bundle {
    val encoded = out port Bits(width bit)
    val count = out port UInt(counterBits bit).setAsReg().init(0)

    val en = in port Bool()
    val reset = in port Bool()
  }

  val counterChunks = io.count.subdivideIn(chunkBits - 1 bit)
  val willOverflowPre = counterChunks.map(c => c.andR)
  val willOverflow = willOverflowPre.asBits().orCum
  val valid = Reg(Bits(validBits bit)) init 0

  when(io.en) {
    io.count := io.count + 1 // TODO fix overflow, keep it to try the formal proof
    valid := valid | willOverflow
  }

  when(io.reset) {
    io.count := 0
    valid := 0
  }

  io.encoded := Cat(counterChunks.indices.map(i => Cat(counterChunks(i), valid(i))))
}

case class LEB128CompressorGenerics(
    dataWidth: Int,
    internalWidth: Int,
    externalTriggerCnt: Int = 0
) {
  val chunkSize = 8
  val chunkCnt = (internalWidth + chunkSize - 1) / chunkSize
  val flagBitCnt = chunkCnt
  val counterWidth = internalWidth - flagBitCnt - dataWidth
  assert(counterWidth > 0, "impossible configuration, no space left for counter")

  def counterBoundaries = {
    val dataRemainder = dataWidth % (chunkSize - 1)
    println("dataRemainder", dataRemainder)
    var nextChunk = chunkSize - 1 - dataRemainder
    var i = 0
    List(0).iterator ++
      Iterator
        .continually {
          i = i + nextChunk
          nextChunk = chunkSize - 1
          i min (counterWidth - 1)
        }
        .takeWhile(_ => i < internalWidth)
  }
}

case class LEB128CompressedData(width: Int) extends Bundle {
  val compressed = Bits(width bit)
  def valid = ???
}

case class LEB128Compressor(g: LEB128CompressorGenerics) extends Component {
  val io = new Bundle {
    val data = in Bits (g.dataWidth bits)
    val compressed = master(Flow(LEB128CompressedData(g.internalWidth)))
    val run = in Bool ()
  }

  val dataChange = io.data =/= RegNext(io.data)

  val counter = Reg(UInt(g.counterWidth bit)) init 0
  println(g.chunkCnt, g.flagBitCnt, g.counterWidth)
  println(g.counterBoundaries.mkString("  "))
  val counterFlagBits = Reg(Bits(g.counterBoundaries.length - 1 bit)) init 0 // MSB flag bit is always 0
  val extractedFlagBits = Vec(g.counterBoundaries.drop(1).map(i => counter(i))).asBits
  val nextCounterFlagBits = counterFlagBits | extractedFlagBits

  val maxCount = counter === counter.maxValue
  when(io.run && !dataChange && !maxCount) {
    counter := counter + 1
    counterFlagBits := nextCounterFlagBits
  } otherwise {
    counter := 0
    counterFlagBits := 0
  }

  val dataFlagBits = Bits(g.flagBitCnt - widthOf(counterFlagBits) bit).setAll()

  val flagBits = nextCounterFlagBits ## dataFlagBits
  val outputData = counter ## io.data

  io.compressed.valid := dataChange || maxCount
  io.compressed.payload.compressed := Cat(
    outputData.subdivideIn(g.chunkSize-1 bit, strict=false).zip((False ## flagBits).subdivideIn(1 bit)).map {
      case (d, f) => f ## d
    }
  )
}

object MemoryFormatter {
  def makeMultipleOf(v: Int, multiple: Int): Int = {
    ((v + multiple - 1) / multiple) * multiple
  }

  def rotate[A](seq: Seq[A], n: Int): Seq[A] = {
    val nn =
      if (n > 0) n % seq.length else seq.length - (n.abs % seq.length)
    seq.view.takeRight(nn) ++ seq.view.dropRight(nn)
  }.toSeq

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
  ) = new Area {
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
        .muxListDc((indices zip results).map({
          case (i: Int, r: Int) => (i, slicedInput(r))
        }))
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
            (delayedEnables(bufferIdx) && !bufferValid) ? buffers(bufferIdx) | buffers(
              ii
            )
          case (false, true) =>
            (enables(ii) && !bufferValid) ? muxes(muxIdx) | buffers(ii)
          case (true, true) =>
            (enables(ii) && !bufferValid) ? muxes(muxIdx) | ((delayedEnables(
              bufferIdx
            ) && !bufferValid) ? buffers(bufferIdx) | buffers(ii))
        }
      }).reverse
    } else buffers.takeRight(outputBlocks)

    output.payload.assignFromBits(Cat(outputs))
    // TODO think about the more obvious solution of making input.ready
    // depend on output.ready to allow for no delay (instead of all the
    // comb paths) - check why this fails for 24-32
    // input.ready := !bufferValid || output.ready
    input.ready := !bufferValid
    output.valid := bufferValid | (if (noDelay)
                                     enables(lastOutputElement) && input_fire && !bufferValid
                                   else False)
  }
}

case class Analyzer(g: AnalyzerGenerics) extends Component {
  val dmaAxiConfig =
    Axi4Config(addressWidth = 64, dataWidth = 64, useId = false)
  val io = new Bundle {
    val data = in(Bits(g.dataWidth bits))
    val externalTrigger = in port Bits(g.externalTriggerCnt bits)
    val triggerMode = in port Vec(TriggerMode(), g.dataWidth)
    val dmaAxi = master port Axi4WriteOnly(dmaAxiConfig)
    val config = new Bundle {
      val startAddress = in port UInt(dmaAxiConfig.addressWidth bits)
      val endAddress = in port UInt(dmaAxiConfig.addressWidth bits)
      val armTrigger = in port Bool()
      val circular = in port Bool()
    }
    val status = new Bundle {
      val running = out port Bool().setAsReg() init False
      val busy = out port Bool()
      val overflow = out port Bool().setAsReg() init False
    }
  }

  val triggerUnit = SimpleTriggerUnit(g.dataWidth)
  triggerUnit.io.data := io.data
  triggerUnit.io.mode := io.triggerMode
  val doTrigger = (triggerUnit.io.trigger || io.externalTrigger.orR) && io.config.armTrigger
  io.status.running.setWhen(doTrigger).clearWhen(io.status.overflow || !io.config.armTrigger)

  val compressor = RLECompressor(g)
  compressor.io.run := io.status.running
  compressor.io.data := io.data

  val fifo = new StreamFifo(Bits(g.internalWidth bit), 64)
  val fifoOverflow = Bool()
  fifo.io.push.translateFrom(compressor.io.compressed.toStream(fifoOverflow))((o, i) =>
    o := i.isData ## i.dataOrTime.resize(g.compressedDataWidth)
  )
  val dma = Axi3Dma(dmaAxiConfig)
  dma.io.run := io.status.running
  dma.io.config.startAddress := io.config.startAddress
  dma.io.config.endAddress := io.config.endAddress
  dma.io.config.circular := io.config.circular
  dma.io.data << fifo.io.pop
  dma.io.dataAvailable := fifo.io.occupancy.resized

  dma.io.axi <> io.dmaAxi
  io.status.busy := dma.io.busy // FIXME we also need to consider the FIFO state

  io.status.overflow.setWhen(fifoOverflow).clearWhen(!io.config.armTrigger)
}
