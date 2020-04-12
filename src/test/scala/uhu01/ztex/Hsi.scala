package innovative_solutions.ztex

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

case class FX3Sim(intf: FX3, block: Seq[Int], clockDomain: ClockDomain)(
    rxCallback: (Int) => Unit
) {
  intf.empty_n #= block.size > 0
  var idx = 0

  var rd_del0 = false
  var rd_del1 = false
  var rd_del2 = false
  def rxFsm(): Unit = {
    rd_del2 = rd_del1
    rd_del1 = rd_del0
    rd_del0 = !intf.rd_n.toBoolean

    intf.empty_n #= idx < block.size
    if (rd_del1 && (idx < block.size)) { // TODO check that we can keep rd low even if no data is present
      intf.dq.read #= block(idx)
      idx = idx + 1
    }
  }
  clockDomain.onSamplings(rxFsm)

  intf.full_n #= true
  var remainingSpace = 1
  var emptyDelay = 10
  var full_del0 = false
  var full_del1 = false
  var full_del2 = false
  def txFsm(): Unit = {
    if (emptyDelay == 0) {
      remainingSpace = Random.nextInt(5) + 5
      emptyDelay = Random.nextInt(5) + 3
    }
    if (remainingSpace == 0) {
      emptyDelay = emptyDelay - 1
    }
    if (remainingSpace > 0 && !intf.wr_n.toBoolean) {
      remainingSpace = remainingSpace - 1
      rxCallback(
        if (intf.dq.writeEnable.toBoolean) intf.dq.write.toInt else 0xffffffff
      )
    }
    full_del2 = full_del1
    full_del1 = full_del0
    full_del0 = remainingSpace == 0
    intf.full_n #= !full_del2
    println(
      f"${simTime()} ${remainingSpace} ${emptyDelay} ${full_del0} ${full_del1} ${full_del1}"
    )
  }
  clockDomain.onSamplings(txFsm)
}

object HsiSim {
  def main(args: Array[String]) {
    var dut = SimConfig.withWave
      .workspacePath("/c/work/tmp/sim")
      .compile(HsiInterface())

    dut.doSim("write") { dut =>
      SimTimeout(2000 * 10)
      val fx3 = FX3Sim(dut.io.fx3, Range(1, 200), dut.clockDomain) { _ =>
      }

      dut.io.tx.en #= false

      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      val received = new Queue[Integer]
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        //println(f"${simTime()} ${payload.toInt}")
        received += payload.toInt
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.fx3.empty_n.toBoolean == false)
      dut.clockDomain.waitRisingEdge(10)
      assert(received.toSeq == Range(1, 200).toSeq)
    }

    // TODO: block data on transmitter, with random empty/non empty
    // TODO: take care to check case where empty goes away but comes back a cycle later
    // TODO: full backpressure
    // TODO: check tristate

    dut.doSim("read") { dut =>
      SimTimeout(200 * 10)
      val toSend = 10
      val scoreboard = ScoreboardInOrder[Int]()
      val fx3 = FX3Sim(dut.io.fx3, Range(1, 2), dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.io.tx.en #= true

      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.ref.length < toSend
      }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(scoreboard.ref.length >= toSend)
      dut.clockDomain.waitRisingEdge(20)
      println(f"${scoreboard.ref}")
      println(f"${scoreboard.dut}")
      scoreboard.check()
    }
  }
}
