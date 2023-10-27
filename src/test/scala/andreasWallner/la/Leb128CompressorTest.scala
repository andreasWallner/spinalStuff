package andreasWallner.la

import andreasWallner.Utils.memoize
import andreasWallner.sim._
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import spinal.core.sim._
import spinal.lib.sim._

class Leb128CompressorTest extends SpinalFunSuite {
  // TODO remove once simRandom is released
  val simRandom = scala.util.Random
  lazy val dutFactory: ((Int, Int, Boolean)) => SimCompiled[LEB128Compressor] =
    memoize {
      case (dw, iw, can) =>
        namedSimConfig.withVcdWave.compile(
          LEB128Compressor(LEB128CompressorGenerics(dw, iw, compressCanonically = can))
            .setDefinitionName(f"LEB128Compressor_${dw}_${iw}_$can")
        )
    }

  for ((dataWidth, internalWidth) <- Seq((3,8), (14,32), (16,32));
       canonical <- List(true, false)) {
    test(
      dutFactory(dataWidth, internalWidth, canonical),
      f"random $dataWidth $internalWidth $canonical"
    ) { dut =>
      val scoreboard = ScoreboardInOrder[(BigInt, BigInt)]()
      FlowMonitor(dut.io.compressed, dut.clockDomain) { v =>
        scoreboard.pushDut(decompress_split(v.compressed.toBigInt, dataWidth, canonical))
      }

      dut.io.run #= false
      val initialData = dut.io.data.randomize()
      scoreboard.pushRef(((BigInt(1) << dut.g.counterWidth) - 1, initialData))

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      dut.io.run #= true
      val possibleRanges = timeBounds(dut.g)
      for (i <- 0 until 100) {
        val range = possibleRanges(simRandom.nextInt(possibleRanges.length))
        val delay = simRandom.between(range._1, range._2)
        dut.clockDomain.waitSampling(delay)
        val data = dut.io.data.changedBigInt()

        scoreboard.pushRef(delay - 1, data) // -1 since 0 means 1 cycle...
      }

      // can't wait for long otherwise counter may run over
      // and DUT sends unexpected output
      dut.clockDomain.waitSampling(1)
      scoreboard.checkEmptyness()
    }

    test(
      dutFactory(dataWidth-1, internalWidth, canonical),
      f"no change $dataWidth $internalWidth $canonical"
    ) { dut =>
      val expectedData = dut.io.data.randomize()
      val expectedTime = (BigInt(1) << dut.g.counterWidth) - 1
      dut.io.run #= true

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge()

      for (_ <- 0 until 2) {
        dut.clockDomain.waitSamplingWhere(dut.io.compressed.valid.toBoolean)

        val (time, data) =
          decompress_split(dut.io.compressed.payload.compressed.toBigInt, dataWidth-1, canonical)
        assert(data == expectedData)
        assert(time == expectedTime)
      }
    }
  }

  // generate a list of time ranges matching to the sizes of the counter
  // chunks so that we get good coverage across all options w/o running for ages
  def timeBounds(g: LEB128CompressorGenerics): Seq[(Int, Int)] = {
    val boundaries = g.counterBoundaries.toList.appended(g.counterWidth)
    boundaries.zip(boundaries.drop(1)).map {
      case (l, h) =>
        val lower = 1 << l
        val higher = (1 << h).min(lower + 4000) // some limit to the range to save some simtime
        (lower, higher)
    }
  }

  // decompress LEB encoding and split into timedelta and data
  def decompress_split(
      compressed: BigInt,
      dataSize: Int,
      checkMinSize: Boolean
  ): (BigInt, BigInt) = {
    // MSB is put into index 0
    val bytes = compressed.toByteArray
    if ((bytes(0) & 0x80) != 0) {
      throw new Exception(
        f"LEB128: invalid MSB: ${compressed.toString(2).reverse.grouped(8).mkString("_").reverse}"
      )
    }

    val result = bytes.foldLeft(BigInt(0))((result, b) => {
      (checkMinSize, result == 0, (b & 0x80) != 0) match {
        case (_, false, false) =>
          throw new Exception(
            f"LEB128: invalid encoding: ${compressed.toString(2).reverse.grouped(8).mkString("_").reverse}"
          )
        case (true, true, true) =>
          throw new Exception(
            f"LEB128: non-canonical: ${compressed.toString(2).reverse.grouped(8).mkString("_").reverse} (${(b.toInt & 0xff).toBinaryString})"
          )
        case _ => ()
      }
      (result << 7) + (b & 0x7f)
    })

    //simLog(compressed.toString(2).reverse.grouped(8).mkString("_").reverse)
    //simLog(result.toString(2).reverse.grouped(8).mkString("_").reverse)
    //simLog(f"$compressed%x, ${result >> dataSize}%x, ${result & ((BigInt(1) << dataSize) - 1)}%x")
    (result >> dataSize, result & ((BigInt(1) << dataSize) - 1))
  }
}
