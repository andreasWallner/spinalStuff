package andreasWallner.la

import andreasWallner.SpinalFunSuite
import andreasWallner.Utils.gcd
import andreasWallner.la.sim.DataDriver
import andreasWallner.sim.simLog
import andreasWallner.zynq.helper.printMemory
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi.sim.{Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
import spinal.lib.sim._

import scala.collection.mutable
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

  lazy val dutFactory
  : ((Int, Int, Boolean)) => SimCompiled[MemoryFormatterTester] =
    memoize {
      case (i, o, noDelay) =>
        val d = if (noDelay) "noDelay" else "delay"
        SimConfig.withFstWave.compile(
          MemoryFormatterTester(i, o)
            .setDefinitionName(f"MemoryFormatterTester_${i}_${o}_$d")
        )
    }

  for (inputWidth <- List(16, 24); outputWidth <- List(32, 64);
       noDelay <- List(true, false)) { // TODO test 48/56
    val d = if (noDelay) "noDelay" else "delay"
    val prefix = f"MemoryFormatter $inputWidth-$outputWidth $d "
    test(prefix + "randomized") {
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "randomized") {
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
      dutFactory(inputWidth, outputWidth, noDelay).doSim(
        prefix + "output starved"
      ) { dut =>
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
      dutFactory(inputWidth, outputWidth, noDelay).doSim(
        prefix + "input starved"
      ) { dut =>
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
      dutFactory(inputWidth, outputWidth, noDelay).doSim(prefix + "max flow") {
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

class AnalyzerTest extends SpinalFunSuite {
  val generics = AnalyzerGenerics(dataWidth = 8, internalWidth = 16)
  val dut = SimConfig.withFstWave.compile(Analyzer(generics))

  test(dut, "foo", seed = 539207874) { dut =>
    SimTimeout(100000)
    dut.io.data #= 0
    dut.io.externalTrigger #= 0
    dut.io.triggerMode(0) #= TriggerMode.high
    for (i <- 1 until 8)
      dut.io.triggerMode(i) #= TriggerMode.disabled
    dut.io.config.startAddress #= 0x1000
    dut.io.config.endAddress #= 0x1100
    dut.io.config.armTrigger #= false
    dut.io.config.circular #= false
    val memory = mutable.HashMap[BigInt, Byte]()
    val expected = mutable.HashMap[BigInt, Byte]()
    new Axi4WriteOnlySlaveAgent(dut.io.dmaAxi, dut.clockDomain)
    new Axi4WriteOnlyMonitor(dut.io.dmaAxi, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        memory(address) = data
      }
    }

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSampling(100)
    dut.io.config.armTrigger #= true

    val cnt = 5
    var i = 0
    var offset = 0
    // start sequence makes sure that first change triggers
    DataDriver(dut.io.data, dut.clockDomain, seqStart = List(0x01)) { (waitTime, newValue) =>
      simLog("DataDriver", waitTime, f"$newValue%02x")
      if (waitTime > 0 && i > 0) {
        expected(0x1000 + offset + 1) = ((waitTime - 1) >> 8).toByte
        expected(0x1000 + offset) = (waitTime - 1).toByte
        offset += 2
      }
      expected(0x1000 + offset + 1) = ((newValue.toBytes(16)(1)) | 0x80).toByte
      expected(0x1000 + offset) = newValue.toBytes(16)(0)
      offset += 2

      i += 1
      i < cnt
    }

    dut.clockDomain.waitSamplingWhere(i >= cnt && !dut.io.status.busy.toBoolean)
    printMemory(memory)
    printMemory(expected)
    assert(memory == expected)
    sleep(1000)
  }
}
