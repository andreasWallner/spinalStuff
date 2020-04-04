package innovative_solutions.ztex

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import scala.collection.mutable.Queue

case class FX3Sim(intf: FX3, block: Seq[Int], clockDomain: ClockDomain) {
  intf.empty_n #= false
  var idx = 0
  var rd_del0 = true
  var rd_del1 = true
  var rd_del2 = true
  def recvFsm(): Unit = {
    rd_del0 = !intf.rd_n.toBoolean
    rd_del1 = rd_del0
    rd_del2 = rd_del1

    if (rd_del2 && (idx < block.size)) { // TODO check that we can keep rd low even if no data is present
      intf.dq.read #= block(idx)
      idx = idx + 1
    }

    intf.empty_n #= idx >= block.size
  }

  // TODO tristate checker

  clockDomain.onSamplings(recvFsm)
}

object HsiSim {
  def main(args: Array[String]) {
    var dut = SimConfig.withWave
      .workspacePath("/mnt/c/work/tmp/sim")
      .compile(HsiInterface())

    dut.doSim("write") { dut =>
      SimTimeout(200 * 10)
      val fx3 = FX3Sim(dut.io.fx3, Range(1, 8), dut.clockDomain)

      dut.io.tx.en #= false

      StreamReadyRandomizer(dut.io.rx.data, dut.clockDomain)
      val received = new Queue[Integer]
      StreamMonitor(dut.io.rx.data, dut.clockDomain) { payload =>
        received += payload.toInt
      }
      // dut.io.rx.data.ready #= true

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.fx3.empty_n.toBoolean == true)
      dut.clockDomain.waitRisingEdge(10)
      assert(received == Range(1, 8))
    }
  }
}
