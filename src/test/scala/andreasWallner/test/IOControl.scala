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
    .compile(IOControl())

  test("set all leds") {
    dut.doSim("set all leds") { dut =>
      SimTimeout(100000)
      val q = Queue[Int](0x7f00, 0x0100, 0x0004, 0xffff, 0xffff, 0x0211, 0x0004)
      SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (q.nonEmpty) (true, q.dequeue()) else (false, 0)
      }
      SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
        println(f"${simTime} ${payload}%#x")
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.led.toInt == 0x3FF)
      dut.clockDomain.waitActiveEdgeWhere(!q.nonEmpty)
      dut.clockDomain.waitActiveEdge(200)
    }
  }

  test("uart rxtx single byte") {
    dut.doSim("uart rxtx single byte") { dut =>
      SimTimeout(100000)
      val q = Queue[Int](0x7f00, 0x0100, 0x0200, 0x0000, 0x0001)
      SlaveFifoSimTx(dut.io.fx3, dut.clockDomain) { () =>
        if (q.nonEmpty) (true, q.dequeue()) else (false, 0)
      }
      SlaveFifoSimRx(dut.io.fx3, dut.clockDomain) { payload =>
        println(f"${simTime} ${payload}%#x")
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.fx3.oe_n.toBoolean == false)
      dut.clockDomain.waitActiveEdge(2000)
    }
  }
}
