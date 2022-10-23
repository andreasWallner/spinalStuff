package andreasWallner.la

import andreasWallner.sim._
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import spinal.core.{Bits, Data}
import spinal.core.sim._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}

import scala.annotation.tailrec
import scala.util.Random

case class CompressedValue(isData: Boolean, data_or_time: BigInt) {
  override def toString = {
    val kind = if (isData) "data " else "time"
    s"$kind: $data_or_time"
  }
}

class RLECompressorTest extends SpinalFunSuite {
  val internalWidth = 8
  val dut = SimConfig
    .withWaveOverride("fst")
    .compile(
      RLECompressor(AnalyzerGenerics(dataWidth = 3, internalWidth = internalWidth))
    )
  val maxTime = Math.pow(2, internalWidth-1).intValue() - 1

  test(dut, "random") { dut =>
    dut.io.data #= 0
    dut.io.run #= false

    val scoreboard = ScoreboardInOrder[CompressedValue]()
    FlowMonitor(dut.io.compressed, dut.clockDomain) { v =>
      scoreboard.pushDut(
        CompressedValue(v.isData.toBoolean, v.dataOrTime.toBigInt)
      )
    }
    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSampling(10)
    scoreboard.pushRef(CompressedValue(isData = true, 0))
    dut.io.run #= true

    // push delay in as many outputs as needed
    @tailrec
    def pushTime(time: Int): Unit = {
      scoreboard.pushRef(
        CompressedValue(isData = false, math.min(time - 1, maxTime))
      )
      if (time > (maxTime + 1)) pushTime(time - maxTime - 1)
    }

    for (_ <- 0 to 1000) {
      if (Random.nextInt(100) > 20) { // 20% chance for two changes back-to-back
        // at least on cycle, otherwise we should have taken the other branch
        val waitTime = Random.nextInt(5 * maxTime) + 1
        pushTime(waitTime)
        dut.clockDomain.waitSampling(waitTime)
      }
      val newValue = dut.io.data.changed()
      scoreboard.pushRef(CompressedValue(isData = true, newValue))
      dut.clockDomain.waitSampling()
    }
  }
}
