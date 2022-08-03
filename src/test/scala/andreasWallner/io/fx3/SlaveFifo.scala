package andreasWallner.io.fx3

import andreasWallner.io.fx3.sim._

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

class SlaveFifoMasterTest extends FunSuite {
  val dut = SimConfig.withWave
    .compile(SlaveFifoMaster())

  test("host writes fpga") {
    dut.doSim("host writes fpga") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
      scoreboard.checkEmptyness()
    }
  }
  test("host writes fpga, full backpressure") {
    dut.doSim("host writes fpga, full backpressure") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
      scoreboard.checkEmptyness()
    }
  }
  test("host writes fpga, full throughput") {
    dut.doSim("host writes fpga, full throughput") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
      scoreboard.checkEmptyness()
    }
  }

  test("write, no extra delay") {
    dut.doSim("write, no delay") { dut =>
      val toSend = 200

      SimTimeout(toSend * 5 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
      scoreboard.checkEmptyness()
    }
  }

  test("read") {
    dut.doSim("read") { dut =>
      val toSend = 10000

      SimTimeout(toSend * 15 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
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
      scoreboard.checkEmptyness()
    }
  }
  test("read, full backpressure") {
    dut.doSim("read, full backpressure") { dut =>
      val toSend = 10000

      SimTimeout(toSend * 15 * 10)
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
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
      scoreboard.checkEmptyness()
    }
  }

  test("bidirectional") {
    dut.doSim("bidirectional") { dut =>
      val toSend = 10000
      val toReceive = 10000

      SimTimeout((toSend + toReceive) * 20 * 10)
      val scoreboardTx = ScoreboardInOrder[Int]()
      val fx3tx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
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
      val fx3rx = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
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
      scoreboardTx.checkEmptyness()
      scoreboardRx.checkEmptyness()
    }
  }

  test("auto pktend") {
    dut.doSim("auto pktend") { dut =>
      val toSend = 5

      SimTimeout(toSend * (20 + toSend*10) * 5 * 10)
      var transmitNext = true
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { _ => }
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
      scoreboard.checkEmptyness()
    }
  }
  test("manual pktend") {
    dut.doSim("manual pktend") { dut =>
      val toSend = 5

      SimTimeout(toSend * (20 + toSend*10) * 5 * 10)
      var transmitNext = true
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3rx = SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { _ => }
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
        scoreboard.checkEmptyness()

        transmitNext = true
      }
    }
  }
}
