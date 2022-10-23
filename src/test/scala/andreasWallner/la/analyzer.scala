package andreasWallner.la

import andreasWallner.Utils.gcd
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.funsuite.AnyFunSuite

import scala.language.postfixOps

case class MemoryFormatterTester(in_width: Int, out_width: Int)
  extends Component {
  val io = new Bundle {
    val i = slave(Stream(Bits(in_width bits)))
    val o = master(Stream(Bits(out_width bits)))
  }
  MemoryFormatter(io.i, io.o, gcd(in_width, out_width))
}

class MemoryFormatterTest extends AnyFunSuite {
  import andreasWallner.Utils.memoize

  lazy val dutFactory: ((Int, Int, Boolean)) => SimCompiled[MemoryFormatterTester] =
    memoize { case (i, o, noDelay) =>
      val d = if(noDelay) "noDelay" else "delay"
      SimConfig.withFstWave.compile(
        MemoryFormatterTester(i, o)
          .setDefinitionName(f"MemoryFormatterTester_${i}_${o}_$d"))}

  for (inputWidth <- List(16, 24); outputWidth <- List(32, 64); noDelay <- List(true, false)) { // TODO test 48/56
    val d = if(noDelay) "noDelay" else "delay"
    val prefix = f"MemoryFormatter $inputWidth-$outputWidth $d "
    test(prefix + "randomized") {
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "randomized") { dut =>
        SimTimeout(100000)
        val scoreboard = ScoreboardInOrder[String]()
        StreamDriver(dut.io.i, dut.clockDomain) { payload =>
          payload.randomize()
          true
        }
        StreamMonitor(dut.io.i, dut.clockDomain) { payload =>
          //println(f"IN: ${payload.toInt}%06x")
          payload.toBigInt
            .hexString(inputWidth)
            .grouped(2)
            .foreach(scoreboard.pushRef)
        }

        StreamReadyRandomizer(dut.io.o, dut.clockDomain)
        StreamMonitor(dut.io.o, dut.clockDomain) { payload =>
          //println(f"OUT: ${payload.toBigInt}%016x")
          payload.toBigInt
            .hexString(outputWidth)
            .grouped(2)
            .foreach(scoreboard.pushDut)
        }
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches >= 1000)
      }
    }
    test(prefix + "output starved") {
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "output starved") {
        dut =>
          SimTimeout(100000)
          val scoreboard = ScoreboardInOrder[String]()
          StreamDriver(dut.io.i, dut.clockDomain) { payload =>
            payload.randomize()
            true
          }.transactionDelay = () => 0
          StreamMonitor(dut.io.i, dut.clockDomain) { payload =>
            //println(f"IN: ${payload.toInt}%06x")
            payload.toBigInt
              .hexString(inputWidth)
              .grouped(2)
              .foreach(scoreboard.pushRef)
          }

          StreamReadyRandomizer(dut.io.o, dut.clockDomain)
          StreamMonitor(dut.io.o, dut.clockDomain) { payload =>
            //println(f"OUT: ${payload.toBigInt}%016x")
            payload.toBigInt
              .hexString(outputWidth)
              .grouped(2)
              .foreach(scoreboard.pushDut)
          }
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches >= 1000)
      }
    }
    test(prefix + "input starved") {
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "input starved") {
        dut =>
          SimTimeout(100000)
          val scoreboard = ScoreboardInOrder[String]()
          StreamDriver(dut.io.i, dut.clockDomain) { payload =>
            payload.randomize()
            true
          }
          StreamMonitor(dut.io.i, dut.clockDomain) { payload =>
            //println(f"IN: ${payload.toInt}%06x")
            payload.toBigInt
              .hexString(inputWidth)
              .grouped(2)
              .foreach(scoreboard.pushRef)
          }

          dut.io.o.ready #= true
          StreamMonitor(dut.io.o, dut.clockDomain) { payload =>
            //println(f"OUT: ${payload.toBigInt}%016x")
            payload.toBigInt
              .hexString(outputWidth)
              .grouped(2)
              .foreach(scoreboard.pushDut)
          }
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches >= 1000)
      }
    }
    test(prefix + "max flow") {
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "max flow") { dut =>
        SimTimeout(100000)
        val scoreboard = ScoreboardInOrder[String]()
        StreamDriver(dut.io.i, dut.clockDomain) { payload =>
          payload.randomize()
          true
        }.transactionDelay = () => 0
        StreamMonitor(dut.io.i, dut.clockDomain) { payload =>
          //println(f"IN: ${payload.toInt}%06x")
          payload.toBigInt
            .hexString(inputWidth)
            .grouped(2)
            .foreach(scoreboard.pushRef)
        }

        dut.io.o.ready #= true
        StreamMonitor(dut.io.o, dut.clockDomain) { payload =>
          //println(f"OUT: ${payload.toBigInt}%016x")
          payload.toBigInt
            .hexString(outputWidth)
            .grouped(2)
            .foreach(scoreboard.pushDut)
        }
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitActiveEdgeWhere(scoreboard.matches >= 1000)
      }
    }
  }
}
