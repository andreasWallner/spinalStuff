package andreasWallner.test

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.ScoreboardInOrder

import andreasWallner.ztex.{FX3SimTx, FX3SimRx}

import org.scalatest.FunSuite
import scala.collection.mutable

class HsiLoopbackTestSim extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(HsiLoopbackTest())

  test("test 150") {
    dut.doSim("test") { dut =>
      val toTransceive = 150
      SimTimeout(toTransceive * 2 * 20 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val fx3tx = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        val dataValue = scoreboard.matches + scoreboard.ref.length + 10
        val sendMore = scoreboard.matches + scoreboard.ref.length < toTransceive
        if (sendMore)
          scoreboard.pushRef(dataValue)
        (sendMore, dataValue)
      }

      dut.io.mode #= 0

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == toTransceive)
      dut.clockDomain.waitActiveEdge(100)
      scoreboard.check()
    }
  }
  test("test 30k") {
    dut.doSim("test") { dut =>
      val toTransceive = 30000
      SimTimeout(toTransceive * 2 * 20 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val fx3tx = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        val dataValue = scoreboard.matches + scoreboard.ref.length + 10
        val sendMore = scoreboard.matches + scoreboard.ref.length < toTransceive
        if (sendMore)
          scoreboard.pushRef(dataValue)
        (sendMore, dataValue)
      }

      dut.io.mode #= 0

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == toTransceive)
      dut.clockDomain.waitActiveEdge(100)
      scoreboard.check()
    }
  }
  test("test xorshift 30k") {
    dut.doSim("test") { dut =>
      val toTransceive = 30000
      SimTimeout(toTransceive * 2 * 20 * 10)
      val received = mutable.Queue[Int]()
      val fx3rx = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        received += payload.toInt
      }
      val fx3tx = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        (false, 0)
      }

      dut.io.mode #= 1

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(received.size == toTransceive)
      dut.clockDomain.waitActiveEdge(100)
    }
  }
}
