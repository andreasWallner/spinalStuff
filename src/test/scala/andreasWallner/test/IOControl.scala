package andreasWallner.test

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
import andreasWallner.io.fx3.sim._

class IOControlSim extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(IOControl())

  test("set all leds") {
    dut.doSim("set all leds") { dut =>
      SimTimeout(1000)
      val q = Queue[Int](0x0100, 0x0004, 0xffff, 0xffff)
      val fx3 = SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (q.nonEmpty) (true, q.dequeue()) else (false, 0)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.led.toInt == 0x3FF)
    }
  }
}
