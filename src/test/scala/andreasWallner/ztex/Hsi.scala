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
    Random.nextInt(10)
  }
  var next_block_size: () => Int = () => {
    Random.nextInt(10)
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

    if (remaining_block_delay == 0 && remaining_block_size == 0) {
      remaining_block_delay = next_block_delay()
      remaining_block_size = next_block_size()
    }
    if (remaining_block_size == 0 && remaining_block_delay > 0) {
      remaining_block_delay = remaining_block_delay - 1
    }
    dataLeft0 = buffer_valid
    if ((intf.empty_n.toBoolean && !intf.rd_n.toBoolean) || !buffer_valid) {
      x0 = buffer

      if (remaining_block_size > 0) {
        val (new_empty, new_x) = txCallback()
        buffer_valid = new_empty
        buffer = new_x
        remaining_block_size = remaining_block_size - 1
      } else {
        buffer_valid = false
      }
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

case class OpenDrainMonitor(
    writeEnable: List[(Bool, Boolean)],
    clockDomain: ClockDomain
) {
  var last: List[Boolean] = writeEnable.map(v => false)

  def check() {
    val current = writeEnable.map(v => v._1.toBoolean == v._2)
    val enabledLines = last.zip(current).map(b => b._1 || b._2).count(b => b)
    assert(enabledLines <= 1, f"overlapping enables: last cycle ${last}, current cycle ${current}")
    last = current
  }
  clockDomain.onActiveEdges(check)
}

class HsiSim extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(HsiInterface())

  test("write") {
    dut.doSim("write") { dut =>
      val toSend = 200

      SimTimeout(1000 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (scoreboard.matches + scoreboard.ref.length < toSend) {
          val dataValue = scoreboard.matches + scoreboard.ref.length + 10
          scoreboard.pushRef(dataValue)
          (true, dataValue)
        } else {
          (false, 0)
        }
      }
      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.io.tx.en #= false

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.matches >= toSend
      )
      scoreboard.check()
    }
  }
  test("write, full backpressure") {
    dut.doSim("write, full backpressure") { dut =>
      val toSend = 200

      SimTimeout(1000 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (scoreboard.matches + scoreboard.ref.length < toSend) {
          val dataValue = scoreboard.matches + scoreboard.ref.length + 10
          scoreboard.pushRef(dataValue)
          (true, dataValue)
        } else {
          (false, 0)
        }
      }
      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      fx3.next_block_size = () => { toSend }
      dut.io.tx.en #= false

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.matches >= toSend
      )
      scoreboard.check()
    }
  }
  test("write, no extra delay") {
    dut.doSim("write, no delay") { dut =>
      val toSend = 200

      SimTimeout(1000 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        val dataValue = scoreboard.matches + scoreboard.ref.length + 10
        val sendMore = scoreboard.matches + scoreboard.ref.length < toSend
        if (sendMore)
          scoreboard.pushRef(dataValue)
        (sendMore, dataValue)
      }
      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      fx3.next_block_delay = () => { 1 }
      dut.io.tx.en #= false

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.matches >= toSend
      )
      scoreboard.check()
    }
  }
  // TODO: check tristate

  test("read") {
    dut.doSim("read") { dut =>
      val toSend = 200

      SimTimeout(2000 * 10)
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
      dut.clockDomain.waitActiveEdgeWhere(
        scoreboard.matches >= toSend
      )
      dut.clockDomain.waitRisingEdge(40)
      scoreboard.check()
    }
  }

  test("read, full backpressure") {
    dut.doSim("read, full backpressure") { dut =>
      val toSend = 50

      SimTimeout(2000 * 10)
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

  test("bidirectional") {
    dut.doSim("bidirectional") { dut =>
      val toSend = 10000
      val toReceive = 10000

      SimTimeout(1000000 * 10)
      val scoreboardTx = ScoreboardInOrder[Int]()
      val fx3tx = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboardTx.pushDut(payload.toInt)
      }
      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboardTx.matches < toSend
      }.transactionDelay = () => { 0 }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboardTx.pushRef(payload.toInt)
      }

      val scoreboardRx = ScoreboardInOrder[Int]()
      val fx3rx = FX3SimTx(dut.io.fx3, dut.clockDomain) { () =>
        val dataValue = scoreboardRx.matches + scoreboardRx.ref.length + 10
        val sendMore = scoreboardRx.matches + scoreboardRx.ref.length < toReceive
        if (sendMore)
          scoreboardRx.pushRef(dataValue)
        (sendMore, dataValue)
      }
      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        scoreboardRx.pushDut(payload.toInt)
      }

      OpenDrainMonitor(
        List((dut.io.fx3.oe_n, false), (dut.io.fx3.dq.writeEnable, true)),
        dut.clockDomain
      )

      dut.io.tx.en #= false
      dut.clockDomain.forkStimulus(10)
      while (scoreboardTx.matches < toSend || scoreboardRx.matches < toReceive) {
        dut.io.tx.en #= !dut.io.tx.en.toBoolean
        dut.clockDomain.waitRisingEdge(Random.nextInt(40))
      }
      scoreboardTx.check()
      scoreboardRx.check()
    }
  }
}
