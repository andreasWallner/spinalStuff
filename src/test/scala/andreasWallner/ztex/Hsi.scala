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

case class SimHistory[T](depth: Int, initial: T = None) {
  var data: Seq[T] = List.fill(depth)(initial)

  def apply(idx: Int) = data(idx)
  def update(next: T) = {
    data = List(next) ++ data.slice(0, depth - 1)
  }
}

case class FX3SimTx(intf: FX3, clockDomain: ClockDomain)(
    txCallback: () => (Boolean, Int)
) {
  var next_block_delay: () => Int = () => {
    Random.nextInt(10) + 2
  }
  var next_block_size: () => Int = () => {
    Random.nextInt(10)
  }

  // txCallback -> data to buffer if empty, then use this 
  intf.empty_n #= false
  var remaining_block_size = 0
  var remaining_block_delay = 0
  var buffer_valid = false
  var buffer = 0
  var empty_n_next = false
  var last_rd_n = true
  def rxFsm(): Unit = {
    if (remaining_block_delay == 0 && remaining_block_size == 0) {
      remaining_block_delay = next_block_delay()
      remaining_block_size = next_block_size()
    }
    if (remaining_block_size == 0 && remaining_block_delay > 0) {
      remaining_block_delay = remaining_block_delay - 1
    }
    if (!buffer_valid) {
      val (valid, x) = txCallback()
      buffer_valid = valid
      buffer = x
    }
    intf.empty_n #= empty_n_next
    if ((intf.empty_n.toBoolean && !last_rd_n && remaining_block_size > 0)) {
      remaining_block_size = remaining_block_size - 1

      intf.dq.read #= buffer
      
      val (valid, x) = txCallback()
      buffer_valid = valid
      buffer = x
      empty_n_next = buffer_valid && (remaining_block_size > 0)
    } else {
      empty_n_next = buffer_valid && (remaining_block_size > 0)
    }
    last_rd_n = intf.rd_n.toBoolean
  }
  clockDomain.onActiveEdges(rxFsm)
}

case class FX3SimRx(intf: FX3, clockDomain: ClockDomain)(
    rxCallback: (Int) => Unit
) {
  var pktendCallback: (Queue[Int]) => Unit = (_) => {}
  var next_remaining_space: () => Int = () => {
    // use a minimum of 4 as worst case
    // in practice the memory is much bigger, but smaller
    // values produce weird special cases in simulation that
    // can't happen in reality
    // (having to retransmit stuff from retransmit buffer)
    Random.nextInt(10) + 4
  }
  var next_empty_delay: () => Int = () => {
    // with a delay of 3 cycles until state is shown, we can't have less
    // than 4 cycles of delay as a torture test, in practise we will have
    // more as this is done in software
    Random.nextInt(5) + 4
  }

  intf.full_n #= true
  var remainingSpace = 1
  var emptyDelay = 10
  val full = SimHistory(4, false)
  var packetBuffer = Queue[Int]()
  def txFsm(): Unit = {
    val currentRemainingSpace = remainingSpace
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
      packetBuffer += intf.dq.write.toInt
    }
    if (currentRemainingSpace > 0 && (!intf.pktend_n.toBoolean || remainingSpace == 0)) {
      pktendCallback(packetBuffer)
      packetBuffer.clear()
      remainingSpace = 0
    }
    full.update(remainingSpace > 0)
    intf.full_n #= full(3)
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

  test("host writes fpga") {
    dut.doSim("host writes fpga") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
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
  test("host writes fpga, full backpressure") {
    dut.doSim("host writes fpga, full backpressure") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
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
  test("host writes fpga, full throughput") {
    dut.doSim("host writes fpga, full throughput") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
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
      dut.io.rx.data.ready #= true
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

      SimTimeout(toSend * 5 * 10)
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

  test("read") {
    dut.doSim("read") { dut =>
      val toSend = 10000

      SimTimeout(toSend * 15 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.length < toSend
      }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }

      dut.io.tx.en #= true
      dut.io.tx.pktend_timeout #= 0

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
      val toSend = 10000

      SimTimeout(toSend * 15 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3SimRx(dut.io.fx3, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.length < toSend
      }.transactionDelay = () => { 0 }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }

      dut.io.tx.en #= true
      dut.io.tx.pktend_timeout #= 0
      dut.io.tx.pktend #= false

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

      SimTimeout((toSend + toReceive) * 20 * 10)
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
      dut.io.tx.pktend_timeout #= 0
      dut.io.tx.pktend #= false

      dut.clockDomain.forkStimulus(10)
      while (scoreboardTx.matches < toSend || scoreboardRx.matches < toReceive) {
        dut.io.tx.en #= !dut.io.tx.en.toBoolean
        dut.clockDomain.waitRisingEdge(Random.nextInt(40))
      }
      scoreboardTx.check()
      scoreboardRx.check()
    }
  }

  test("auto pktend") {
    dut.doSim("auto pktend") { dut =>
      val toSend = 5

      SimTimeout(toSend * (20 + toSend*10) * 5 * 10)
      var transmitNext = true
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = FX3SimRx(dut.io.fx3, dut.clockDomain) { _ => }
      fx3rx.pktendCallback = { (values: Queue[Int]) =>
        assert(values.size == 1)
        scoreboard.pushDut(values(0))
        transmitNext = true
        dut.io.tx.pktend_timeout #= dut.io.tx.pktend_timeout.toInt + 10
      }
      fx3rx.next_remaining_space = () => { 100 }
      fx3rx.next_empty_delay = () => { 20 }
      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        val currentTransmitNext = transmitNext
        payload.randomize()
        transmitNext = false
        currentTransmitNext
      }.transactionDelay = () => { 0 }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }
      dut.io.tx.pktend_timeout #= 20
      dut.io.tx.en #= true
      dut.io.tx.pktend #= false

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere({
        scoreboard.matches >= toSend
      })
      dut.clockDomain.waitRisingEdge(40)
      scoreboard.check()
    }
  }
  test("manual pktend") {
    dut.doSim("manual pktend") { dut =>
      val toSend = 5

      SimTimeout(toSend * (20 + toSend*10) * 5 * 10)
      var transmitNext = true
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = FX3SimRx(dut.io.fx3, dut.clockDomain) { _ => }
      fx3rx.pktendCallback = { (values: Queue[Int]) =>
        assert(values.size == 1)
        scoreboard.pushDut(values(0))
      }
      fx3rx.next_remaining_space = () => { 100 }
      fx3rx.next_empty_delay = () => { 20 }
      // initial remainingSpace is otherwise 1
      //  -> would end block before manual end can be triggered
      fx3rx.remainingSpace = 20
      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        val currentTransmitNext = transmitNext
        payload.randomize()
        transmitNext = false
        currentTransmitNext
      }.transactionDelay = () => { 0 }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }
      dut.io.tx.pktend_timeout #= 0
      dut.io.tx.en #= true
      dut.io.tx.pktend #= false

      dut.clockDomain.forkStimulus(10)
      while(scoreboard.matches < toSend) {
        dut.clockDomain.waitActiveEdge(40);
        dut.io.tx.pktend #= true
        dut.clockDomain.waitActiveEdge();
        dut.io.tx.pktend #= false

        dut.clockDomain.waitActiveEdgeWhere(dut.io.tx.pktend_done.toBoolean == true)
        dut.clockDomain.waitActiveEdge(10)
        scoreboard.check()

        transmitNext = true
      }
    }
  }
}
