package andreasWallner.io.iso7816

import andreasWallner.io.iso7816.sim._
import andreasWallner.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.lib.sim.{
  FlowMonitor,
  ScoreboardInOrder,
  StreamDriver,
  StreamMonitor
}

import scala.util.Random

class TxRxCoreSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .compile(ISO7816Master())

  test("TX charrep") {
    // Master TX with character repetition enabled
    // Verify that:
    //  - bytes are resent if card indicates an error
    //  - the master driver is never enabled where the card would drive
    dut.doSim("TX charrep") { dut =>
      val toSend = 500
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.config.characterRepetition #= true
      dut.io.config.cgt #= 11
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.clockDomain.forkStimulus(10)

      dut.io.iso.io.simulatePullup()
      dut.io.iso.io.prohibitAnyConcurrentDrivers()

      val scoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.size < toSend
      }
      StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }
      ISO7816SimRx(dut.io.iso, 100) { (data, parityValid) =>
        assert(parityValid)
        if (Random.nextBoolean()) {
          scoreboard.pushDut(data)
          true
        } else {
          false
        }
      }

      dut.clockDomain.waitActiveEdgeWhere(dut.io.tx.valid.toBoolean)
      dut.io.start.tx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.tx #= false
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == toSend)
      scoreboard.checkEmptyness()
      assert(scoreboard.matches == toSend)
    }
  }

  test("RX charrep") {
    // Master receive with character repetition enabled
    // Verify that
    //  - error is indicated if parity is invalid
    //  - wrong data is not forwarded to RX flow
    //  - the master driver is never enabled where the card would drive
    dut.doSim("RX charrep") { dut =>
      val toSend = 500
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.config.characterRepetition #= true
      dut.io.config.cgt #= 11
      dut.io.start.tx #= false
      dut.io.start.rx #= false

      dut.io.iso.io.simulatePullup()
      dut.io.iso.io.prohibitAnyConcurrentDrivers()

      val scoreboard = ScoreboardInOrder[Int]()
      FlowMonitor(dut.io.rx, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val isosim = ISO7816SimTx(dut.io.iso, 100) { (data, error, induceError) =>
        assert(error == induceError)
        if (!error)
          scoreboard.pushRef(data)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(10)
      dut.io.start.rx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.rx #= false

      for (_ <- 1 to toSend)
        isosim.txByte(Random.nextInt(0x100), Random.nextBoolean())

      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.dut.isEmpty && scoreboard.ref.isEmpty
      )
      scoreboard.checkEmptyness()
    }
  }

  test("TX no-charrep") {
    // Master transmit with character repetition disabled
    // Verify that:
    //  - error signal is ignored
    //  - no data is resent
    dut.doSim("TX no-charrep") { dut =>
      val toSend = 500
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.config.characterRepetition #= false
      dut.io.config.cgt #= 14 // increase to make sure TX does not interfere with unexpected error
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.clockDomain.forkStimulus(10)

      dut.io.iso.io.simulatePullup()

      val scoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.size < toSend
      }
      StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }
      ISO7816SimRx(dut.io.iso, 100) { (data, parityValid) =>
        assert(parityValid)
        scoreboard.pushDut(data)
        Random.nextBoolean()
      }

      dut.clockDomain.waitActiveEdgeWhere(dut.io.tx.valid.toBoolean)
      dut.io.start.tx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.tx #= false
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == toSend)
      scoreboard.checkEmptyness()
      assert(scoreboard.matches == toSend)
    }
  }

  test("RX no-charrep") {
    // Master receive with character repetition disabled
    // Verify that:
    //  - error signal is not generated
    //  - data is forwarded to RX flow in any case
    dut.doSim("RX no-charrep") { dut =>
      val toSend = 500
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.config.characterRepetition #= false
      dut.io.config.cgt #= 14 // increase to make sure TX does not interfere with unexpected error
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.clockDomain.forkStimulus(10)

      dut.io.iso.io.simulatePullup()

      val scoreboard = ScoreboardInOrder[Int]()
      FlowMonitor(dut.io.rx, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val isosim = ISO7816SimTx(dut.io.iso, 100) { (data, error, induceError) =>
        assert(!error)
        scoreboard.pushRef(data)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(10)
      dut.io.start.rx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.rx #= false

      for (_ <- 1 to toSend)
        isosim.txByte(Random.nextInt(0x100), Random.nextBoolean())

      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.dut.isEmpty && scoreboard.ref.isEmpty
      )
      scoreboard.checkEmptyness()

    }
  }

  // TODO: test cgt
}
