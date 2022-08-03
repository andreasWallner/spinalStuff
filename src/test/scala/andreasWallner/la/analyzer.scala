package andreasWallner.la

import spinal.core._
import spinal.sim._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.FunSuite

case class MemoryFormatterTester(in_width: Int, out_width: Int) extends Component {
  val io = new Bundle {
    val i = slave(Stream(Bits(in_width bits)))
    val o = master(Stream(Bits(out_width bits)))
  }
  MemoryFormatter(io.i, io.o)
}

class MemoryFormatterTest extends FunSuite {
  val dut = SimConfig.withWave
    .compile(MemoryFormatterTester(24, 64))
  
  test("MemoryFormatter 24-64") {
    dut.doSim("MemoryFormatter 24-64", seed=1664140653) { dut =>
      SimTimeout(10000)
      val scoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.i, dut.clockDomain) {
        payload => payload.randomize
        true
      }
      StreamMonitor(dut.io.i, dut.clockDomain) { payload =>
        println(f"IN: ${payload.toInt}%06x")
        scoreboard.pushRef((payload.toInt >> 16) & 0xff)
        scoreboard.pushRef((payload.toInt >> 8) & 0xff)
        scoreboard.pushRef(payload.toInt & 0xff)
      }

      StreamReadyRandomizer(dut.io.o, dut.clockDomain)
      StreamMonitor(dut.io.o, dut.clockDomain) { payload =>
        println(f"OUT: ${payload.toBigInt}%016x")
        scoreboard.pushDut((payload.toBigInt >> 56).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 48).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 40).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 32).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 24).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 16).toInt & 0xff)
        scoreboard.pushDut((payload.toBigInt >> 8).toInt & 0xff)
        scoreboard.pushDut(payload.toBigInt.toInt & 0xff)        
      }
      dut.clockDomain.forkStimulus(10)
      waitUntil(scoreboard.matches == 100)
    }
  }
}