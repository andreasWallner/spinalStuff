package andreasWallner

import andreasWallner.bus.amba3.ahblite3.AhbLite3Arbiter
import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.{SimString, simLog}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.sim.ScoreboardInOrder

import scala.collection.mutable

class AhbLite3ArbiterTest extends SpinalFunSuite {
  val config = AhbLite3Config(8, 16)
  val dut = namedSimConfig.compile {
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
        var burstAddress = 0
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

            // TODO align
            val maxAddr = ahb.HADDR.maxValue - (if(burst) burstSize * 4 else 0)
            burstAddress = simRandom.between(0, (maxAddr+1).toInt)
            ahb.HADDR #= burstAddress
            ahb.HMASTLOCK #= lock

          } else {
            ahb.HTRANS #= HTRANS.SEQ
            remainingBurst = remainingBurst - 1
            val wrap = (burstType & 1) == 0
            val transferSize = 4
            ahb.HADDR #= (if(!wrap) {
              burstAddress = burstAddress + transferSize
              burstAddress
            } else {
              val boundary = burstSize * transferSize
              val fixed = burstAddress & ~(boundary - 1)
              val newOffset = ((burstAddress - fixed) + transferSize) % boundary
              burstAddress = fixed + newOffset
              burstAddress
            })
            ahb.HBURST #= burstType
          }
          (Some(ahb.HWDATA.randomizedBigInt()), remainingBurst > 0)
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
      AhbLite3ProtocolChecker(dut.io.inputs(i), dut.clockDomain, f"M$i")
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
    AhbLite3ProtocolChecker(dut.io.output, dut.clockDomain, "S")

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSamplingWhere(masterCounts.forall(_ > 1000))
    assert(scoreboard.matches > 2000)
  }
}
