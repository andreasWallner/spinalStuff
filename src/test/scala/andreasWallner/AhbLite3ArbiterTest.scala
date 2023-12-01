package andreasWallner

import andreasWallner.bus.amba3.ahblite3.AhbLite3Arbiter
import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.SimString
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.sim.ScoreboardInOrder

import scala.collection.mutable

class AhbLite3ArbiterTest extends SpinalFunSuite {
  val config = AhbLite3Config(8, 16)
  val dut = namedSimConfig.withVcdWave.compile {
    val c = new Component {
      val io = new Bundle {
        val inputs = Vec(slave port AhbLite3Master(config), 2)
        val output = master port AhbLite3(config)
      }
      val inner = AhbLite3Arbiter(AhbLite3Config(8, 16), 2, roundRobinArbiter = false)
      io.inputs(0).toAhbLite3() >> inner.io.inputs(0)
      io.inputs(1).toAhbLite3() >> inner.io.inputs(1)
      inner.io.output >> io.output

      val masterStrings = Seq.tabulate(2) { i =>
        SimString(s"master_$i").materialize()
      }
      val slaveStrings = Seq.tabulate(1) { i =>
        SimString(s"slave_$i").materialize()
      }
    }
    c
  }
  test(dut, "blub") { dut =>
    //SimTimeout(10000)
    val scoreboard = LoggingScoreboardInOrder[(BigInt, String, BigInt)]()
    val masterCounts = mutable.Seq.fill(2)(0)
    for (i <- 0 until 2) {
      new AhbLite3MasterAgent(dut.io.inputs(i), dut.clockDomain, dut.masterStrings(i)) {
        var burstSize = 0
        var remainingBurst = 0
        var burstType = 0
        override def setupNextTransfer() = {
          if (remainingBurst == 0) {
            val burst = simRandom.nextFloat() > 0.5
            val lock = !burst & simRandom.nextFloat() > 0.5

            if(burst) {
              burstType = simRandom.between(1, 8)
              burstSize = burstType >> 1 match {
                case 0 => simRandom.between(1, 16)
                case 1 => 4
                case 2 => 8
                case 3 => 16
              }
              remainingBurst = burstSize - 1
              ahb.HBURST #= burstType
            }

            val maxAddr = ahb.HADDR.maxValue - (if(burst) (burstSize * 4) else 0)
            val addr = simRandom.between(0, (maxAddr+1).toInt)
            ahb.HADDR #= addr
            ahb.HMASTLOCK #= lock

          } else {
            remainingBurst = remainingBurst - 1
            val wrap = (burstType & 1) == 0
            val transferSize = 4
            val boundary = burstSize * transferSize
            ahb.HADDR #= (if(!wrap) {
              ahb.HADDR.toBigInt + transferSize
            } else {
              ahb.HADDR.toBigInt + transferSize // TODO wrap!
            })
            ahb.HBURST #= burstType
          }
          Some(ahb.HWDATA.randomizedBigInt())
        }
      }
      new AhbLite3MasterMonitor(dut.io.inputs(i), dut.clockDomain, i) {
        override def onRead(address: BigInt, value: BigInt): Unit = {
          scoreboard.pushRef((address, "R", value))
          masterCounts(i) = masterCounts(i) + 1
        }

        override def onWrite(address: BigInt, value: BigInt): Unit = {
          scoreboard.pushRef((address, "W", value))
          masterCounts(i) = masterCounts(i) + 1
        }
      }
    }

    new AhbLite3SlaveAgent(
      dut.io.output,
      dut.clockDomain,
      dut.slaveStrings.head,
      None
    ) {
      override def onRead(address: BigInt) = (ahb.HRDATA.randomizedBigInt(), false)

      override def onWrite(address: BigInt, value: BigInt) = false
    }
    new AhbLite3SlaveMonitor(dut.io.output, dut.clockDomain) {
      override def onRead(address: BigInt, value: BigInt): Unit =
        scoreboard.pushDut((address, "R", value))

      override def onWrite(address: BigInt, value: BigInt): Unit =
        scoreboard.pushDut((address, "W", value))
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSamplingWhere(masterCounts.head > 500 && masterCounts(1) > 500)
    assert(scoreboard.matches > 100)
  }
}
