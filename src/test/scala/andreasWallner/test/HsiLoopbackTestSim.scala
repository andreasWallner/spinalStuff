package andreasWallner.test

import andreasWallner.SpinalFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.ScoreboardInOrder
import andreasWallner.io.fx3.sim._

import scala.collection.mutable
import scala.language.postfixOps

class HsiLoopbackTestSim extends SpinalFunSuite {
  val dut = SimConfig.withWave
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    )
    .compile(SlaveFifoLoopback())

  test(dut, "test 150") { dut =>
    val toTransceive = 150
    SimTimeout(toTransceive * 2 * 20 * 10)
    val scoreboard = ScoreboardInOrder[Int]()
    SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
      scoreboard.pushDut(payload)
    }
    SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
    scoreboard.checkEmptyness()
  }

  test(dut, "test 30k") { dut =>
    val toTransceive = 30000
    SimTimeout(toTransceive * 2 * 20 * 10)
    val scoreboard = ScoreboardInOrder[Int]()
    SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
      scoreboard.pushDut(payload)
    }
    SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
    scoreboard.checkEmptyness()
  }

  test(dut, "test xorshift 30k") { dut =>
    val toTransceive = 30000
    SimTimeout(toTransceive * 2 * 20 * 10)
    val received = mutable.Queue[Int]()
    val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
      received += payload
    }
    fx3rx.next_remaining_space = () => 64000
    SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
      (false, 0)
    }

    dut.io.mode #= 1

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitActiveEdgeWhere(received.size == toTransceive)
    dut.clockDomain.waitActiveEdge(100)
  }
}
