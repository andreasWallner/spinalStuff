package andreasWallner.la

import andreasWallner.Utils.memoize
import andreasWallner.sim._
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import spinal.core.sim._
import spinal.lib.sim._

class Leb128CompressorTest extends SpinalFunSuite {
  def decompress_split(b: BigInt, dataSize: Int): (BigInt, BigInt) = {
    var compressed = b
    var result = compressed & 0x7f
    compressed = compressed >> 7
    var shift = 7
    while (compressed != 0) {
      if ((compressed & 1) == 0)
        throw new Exception(f"invalid encoding: ${b.toString(2).reverse.grouped(8).mkString("_").reverse}")
      compressed = compressed >> 1
      result = result | ((compressed & 0x7f) << shift)
      shift = shift + 7
      compressed = compressed >> 7
    }

    simLog(f"$b%x, ${result >> dataSize}%x, ${result & ((1 << dataSize) - 1)}%x")
    (result >> dataSize, result & ((1 << dataSize) - 1))
  }

  lazy val dutFactory
  : ((Int, Int)) => SimCompiled[LEB128Compressor] =
    memoize {
      case (dw, iw) =>
        namedSimConfig.compile(
          LEB128Compressor(LEB128CompressorGenerics(dw, iw))
            .setDefinitionName(f"LEB128Compressor_${dw}_$iw")
        )
    }
  /*val dut = namedSimConfig.compile(
    LEB128Compressor(LEB128CompressorGenerics(dataWidth = 3, internalWidth = 16))
  )*/

  test(dutFactory(3, 16), "test") { dut =>
    val scoreboard = LoggingScoreboardInOrder[(BigInt, BigInt)]()
    FlowMonitor(dut.io.compressed, dut.clockDomain) { v =>
      scoreboard.pushDut(decompress_split(v.compressed.toBigInt, 3))
    }

    dut.clockDomain.forkStimulus(10)
    dut.io.run #= true
    dut.io.data #= 1
    scoreboard.pushRef(0, 1)
    for (i <- 0 until 10) {
      val range = List((1, 16), (16, 512))(simRandom.nextInt(1))
      val delay = simRandom.between(range._1, range._2)

      dut.clockDomain.waitSampling(delay)
      val data = dut.io.data.changedBigInt()

      scoreboard.pushRef(delay - 1, data) // -1 since 0 means 1 cycle...
    }

    dut.clockDomain.waitSampling(10)
    scoreboard.checkEmptyness()
  }

  test(dutFactory(10, 32), "large", 2080416477) { dut =>
    val scoreboard = LoggingScoreboardInOrder[(BigInt, BigInt)]()
    FlowMonitor(dut.io.compressed, dut.clockDomain) { v =>
      scoreboard.pushDut(decompress_split(v.compressed.toBigInt, 10))
    }

    dut.clockDomain.forkStimulus(10)
    dut.io.run #= true
    dut.io.data #= 1
    scoreboard.pushRef(0, 1)
    for (i <- 0 until 10) {
      val range = List((1, 16), (16, 2048), (2048, 8192))(simRandom.nextInt(2))
      val delay = simRandom.between(range._1, range._2)

      dut.clockDomain.waitSampling(delay)
      val data = dut.io.data.changedBigInt()

      scoreboard.pushRef(delay - 1, data) // -1 since 0 means 1 cycle...
    }

    dut.clockDomain.waitSampling(10)
    scoreboard.checkEmptyness()
  }
}
