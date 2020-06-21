package andreasWallner.test

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.ScoreboardInOrder

import andreasWallner.io.fx3.sim._

import org.scalatest.FunSuite
import scala.collection.mutable

class HsiLoopbackTestSim extends FunSuite {
  val dut = SimConfig.withWave
    .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
    .workspacePath("/c/work/tmp/sim")
    .compile(SlaveFifoLoopback())

  test("test 150") {
    dut.doSim("test 150") { dut =>
      val toTransceive = 150
      SimTimeout(toTransceive * 2 * 20 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val fx3tx = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
    dut.doSim("test 30k") { dut =>
      val toTransceive = 30000
      SimTimeout(toTransceive * 2 * 20 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val fx3tx = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
    dut.doSim("test xorshift 30k") { dut =>
      val toTransceive = 30000
      SimTimeout(toTransceive * 2 * 20 * 10)
      val received = mutable.Queue[Int]()
      val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
        received += payload.toInt
      }
      fx3rx.next_remaining_space = () => 64000
      val fx3tx = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
        (false, 0)
      }

      dut.io.mode #= 1

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(received.size == toTransceive)
      dut.clockDomain.waitActiveEdge(100)
    }
  }
}
