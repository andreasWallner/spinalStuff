package andreasWallner.spinaltap

import andreasWallner.ztex.SocketArbiter
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.Queue
import scala.util.Random
import scala.math.min

class SocketArbiterSim extends AnyFunSuite {
  val socketCnt = 3
  val dut = SimConfig.withWave
    .compile(SocketArbiter(socketCnt))

  test("write registers") {
    dut.doSim("write registers") { dut =>
      val toWrite = 200
      SimTimeout(toWrite * 40 * 10)

      val scoreboards = for (i <- 0 until socketCnt) yield ScoreboardInOrder[Int]()
      val q = Queue[Int]()
      var enqueued = 0
      StreamDriver(dut.io.all, dut.clockDomain) { payload =>
        if (q.nonEmpty) {
          payload #= q.dequeue()
          true
        } else if (enqueued < toWrite) {
          val len = min(toWrite - enqueued, Random.nextInt(5))
          val socket = Random.nextInt(socketCnt)
          enqueued += len
          payload #= (socket << 8) + len
          for (_ <- 1 to len) {
            val r = Random.nextInt(65536)
            q += r
            scoreboards(socket).pushRef(r)
          }
          true
        } else {
          false
        }
      }
      for (i <- 0 until socketCnt) {
        StreamReadyRandomizer(dut.io.sockets(i), dut.clockDomain)
        StreamMonitor(dut.io.sockets(i), dut.clockDomain) { payload =>
          scoreboards(i).pushDut(payload.toInt)
        }
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdgeWhere(
        scoreboards.map(_.matches).sum >= toWrite
      )
      dut.clockDomain.waitActiveEdge(50)
      for (sb <- scoreboards)
        sb.checkEmptyness()
    }
  }
}
