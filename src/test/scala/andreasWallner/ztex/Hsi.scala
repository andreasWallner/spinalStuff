package andreasWallner.ztex

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{
  StreamDriver,
  StreamMonitor,
  StreamReadyRandomizer,
  ScoreboardInOrder
}
import scala.collection.mutable.Queue
import scala.util.Random
import org.scalatest.FunSuite

case class FX3SimTx(intf: FX3, clockDomain: ClockDomain)(
    txCallback: () => (Boolean, Int)
) {
  var next_block_delay: () => Int = () => {
    5
  }
  var next_block_size: () => Int = () => {
    5
  }

  intf.empty_n #= false
  var remaining_block_size = 0
  var remaining_block_delay = 0
  var x0 = 0
  var dataLeft0 = false
  var buffer = 0
  var buffer_valid = false
  def rxFsm(): Unit = {
    intf.empty_n #= dataLeft0
    intf.dq.read #= x0

    dataLeft0 = buffer_valid
    if ((intf.empty_n.toBoolean && !intf.rd_n.toBoolean) || !buffer_valid) {
      x0 = buffer
      
      val (new_empty, new_x) = txCallback()
      buffer_valid = new_empty
      buffer = new_x
    }
  }
  clockDomain.onActiveEdges(rxFsm)
}

case class FX3SimRx(intf: FX3, clockDomain: ClockDomain)(
    rxCallback: (Int) => Unit
) {
  var next_remaining_space: () => Int = () => {
    Random.nextInt(5) + 5
  }
  var next_empty_delay: () => Int = () => {
    Random.nextInt(5) + 3
  }

  intf.full_n #= true
  var remainingSpace = 1
  var emptyDelay = 10
  var full_del0 = false
  var full_del1 = false
  var full_del2 = false
  var full_del3 = false
  def txFsm(): Unit = {
    if (emptyDelay == 0) {
      remainingSpace = next_remaining_space()
      emptyDelay = next_empty_delay()
    }
    if (remainingSpace == 0) {
      emptyDelay = emptyDelay - 1
    }
    if (remainingSpace > 0 && !intf.wr_n.toBoolean) { // TODO should we check that we also indicate not-full?
      remainingSpace = remainingSpace - 1
      rxCallback(
        if (intf.dq.writeEnable.toBoolean) intf.dq.write.toInt else 0xffffffff
      )
    }
    intf.full_n #= !full_del2
    full_del2 = full_del1
    full_del1 = full_del0
    full_del0 = remainingSpace == 0
  }
  clockDomain.onActiveEdges(txFsm)
}

class HsiSim extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(HsiInterface())

  test("write") {
    dut.doSim("write") { dut =>
      SimTimeout(500 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      var idx = 0
      val toSend = 100
      val fx3 = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (scoreboard.matches + scoreboard.ref.length < toSend) {
          val dataValue = scoreboard.matches + scoreboard.ref.length + 10
          scoreboard.pushRef(dataValue)
          (true, dataValue)
        } else {
          (false, 0)
        }
      }

      dut.io.tx.en #= false

      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      val received = new Queue[Integer]
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches + scoreboard.ref.length >= toSend)
      dut.clockDomain.waitRisingEdge(20)
      scoreboard.check()
    }
  }

  // TODO: block data on transmitter, with random empty/non empty
  // TODO: take care to check case where empty goes away but comes back a cycle later
  // TODO: full backpressure
  // TODO: check tristate

  test("read") {
    dut.doSim("read") { dut =>
      SimTimeout(2000 * 10)
      val toSend = 200
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.io.tx.en #= true

      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.length < toSend
      }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere({
        scoreboard.matches >= toSend
      })
      dut.clockDomain.waitRisingEdge(40)
      scoreboard.check()
    }
  }

  test("read, full backpressure") {
    dut.doSim("read, full backpressure") { dut =>
      SimTimeout(2000 * 10)
      val toSend = 50
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.io.tx.en #= true

      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.length < toSend
      }.transactionDelay = () => { 0 }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere({
        scoreboard.matches >= toSend
      })
      dut.clockDomain.waitRisingEdge(40)
      scoreboard.check()
    }
  }
}
