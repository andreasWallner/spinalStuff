package innovative_solutions.ztex

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import scala.collection.mutable.Queue

case class FX3Sim(intf: FX3, block: Seq[Int], clockDomain: ClockDomain)(rxCallback : (Int) => Unit) {
  intf.empty_n #= block.size > 0
  var idx = 0

  var rd_del0 = false
  var rd_del1 = false
  var rd_del2 = false
  def recvFsm(): Unit = {
    rd_del2 = rd_del1
    rd_del1 = rd_del0
    rd_del0 = !intf.rd_n.toBoolean

    intf.empty_n #= idx < block.size
    if (rd_del1 && (idx < block.size)) { // TODO check that we can keep rd low even if no data is present
      intf.dq.read #= block(idx)
      idx = idx + 1
    }
  }

  var wr_del0 = true
  var wr_del1 = true
  var wr_del2 = true
  var dq_write_del0 = 0
  var dq_write_del1 = 0
  var dq_write_del2 = 0
  def txFsm(): Unit = {
    wr_del0 = !intf.wr_n.toBoolean
    wr_del1 = wr_del0
    wr_del2 = wr_del1
    dq_write_del0 = intf.dq.write.toInt
    dq_write_del1 = dq_write_del0
    dq_write_del2 = dq_write_del1

    if(wr_del2) {
      rxCallback(dq_write_del2)
    }
  }

  // TODO tristate checker

  clockDomain.onSamplings(recvFsm)
}

object HsiSim {
  def main(args: Array[String]) {
    var dut = SimConfig.withWave
      .workspacePath("/c/work/tmp/sim")
      .compile(HsiInterface())

    dut.doSim("write") { dut =>
      SimTimeout(2000 * 10)
      val fx3 = FX3Sim(dut.io.fx3, Range(1, 2), dut.clockDomain){_ =>}

      dut.io.tx.en #= false

      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      val received = new Queue[Integer]
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        println(f"${simTime()} ${payload.toInt}")
        received += payload.toInt
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.fx3.empty_n.toBoolean == false)
      dut.clockDomain.waitRisingEdge(10)
      println(f"${received}")
      assert(received.toSeq == Range(1, 200).toSeq)
    }

    // TODO: block data on transmitter, with random empty/non empty
    // TODO: take care to check case where empty goes away but comes back a cycle later
    // TODO: full backpressure

    dut.doSim("read") { dut =>
      SimTimeout(2000 * 10)
      val recvd = Queue[Int]()
      val fx3 = FX3Sim(dut.io.fx3, Range(1, 2), dut.clockDomain) { payload =>
        recvd += payload.toInt
      }

      dut.io.tx.en #= true
      val ref = Queue[Int]()

      var sent = 0
      StreamDriver(dut.io.tx.data, dut.clockDomain) { payload =>
        sent += 1
        if (sent >= 20) {
          false
        } else {
          payload.randomize()
          true
        }
      }
      StreamMonitor(dut.io.tx.data, dut.clockDomain) { payload => 
        ref += payload.toInt
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(sent >= 20)
      dut.clockDomain.waitRisingEdge(10)
      println(f"${ref}")
    }
  }
}
