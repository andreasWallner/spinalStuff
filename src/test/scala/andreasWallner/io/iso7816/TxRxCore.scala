package andreasWallner.io.iso7816

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{
  StreamDriver,
  StreamMonitor,
  StreamReadyRandomizer,
  ScoreboardInOrder
}
import andreasWallner.sim._
import andreasWallner.io.iso7816.sim._

import scala.collection.mutable.Queue
import scala.util.Random
import org.scalatest.FunSuite
import spinal.lib.sim.FlowMonitor

class TxRxCoreSim extends FunSuite {
  val dut = SimConfig.withWave
    .compile(ISO7816Master())

  test("TX") {
    dut.doSim("TX", seed=1) { dut =>
      val toSend = 50
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.clockDomain.forkStimulus(10)
      
      val scoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.size < toSend
      }
      StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
        println(f"${simTime()} ref ${payload.toInt}")
        scoreboard.pushRef(payload.toInt)
      }
      dut.io.iso.io.simulatePullup()
      ISO7816SimRx(dut.io.iso, 100) { (data, parityValid) =>
        assert(parityValid)
        if (Random.nextBoolean()) {
          println(f"${simTime()} dut ${data}")
          scoreboard.pushDut(data)
          true
        } else {
          println(f"${simTime()} erroring")
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

  test("RX") {
    dut.doSim("RX", seed=1203102499) { dut =>
      val toSend = 50
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.io.iso.io.simulatePullup()

      val scoreboard = ScoreboardInOrder[Int]()
      FlowMonitor(dut.io.rx, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }
      val isosim = ISO7816SimTx(dut.io.iso, 100) { (data, error) =>
        assert(!error)
        scoreboard.pushRef(data)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(10)
      dut.io.start.rx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.rx #= false

      for (_ <- 1 to toSend)
        isosim.txByte(Random.nextInt(0x100))

      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches == toSend)
      scoreboard.checkEmptyness()
    }
  }
}
