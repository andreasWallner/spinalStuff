package andreasWallner

import andreasWallner.bus.amba3.ahblite3.AhbLite3Arbiter
import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.{SimString, simLog, simLogId}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.sim.ScoreboardInOrder

import scala.collection.mutable

class AhbLite3ArbiterTest extends SpinalFunSuite {
  for (roundRobinArbiter <- Seq(true, false)) {
    val dutName = s"arbiter_$roundRobinArbiter"
    val sc = namedSimConfig.extendName(dutName)
    val dut = sc.compile {
      val config = AhbLite3Config(8, 16)
      val masterCnt = 2
      new Component {
        val io = new Bundle {
          val inputs = Vec(slave port AhbLite3Master(config), masterCnt)
          val output = master port AhbLite3(config)
        }
        val inner = AhbLite3Arbiter(config, masterCnt, roundRobinArbiter)
        for (i <- 0 until masterCnt) {
          io.inputs(i).toAhbLite3() >> inner.io.inputs(i)
        }
        inner.io.output >> io.output
      }
    }

    test(dut, s"random_$dutName") { dut =>
      SimTimeout(200000)

      val scoreboard = LoggingScoreboardInOrder[(BigInt, String, BigInt)]()
      val masterCounts = mutable.Seq.fill(2)(0)

      for (i <- 0 until 2) {
        new AhbLite3MasterAgent(dut.io.inputs(i), dut.clockDomain) {
          var burstSize = 0
          var remainingBurst = 0
          var burstType = 0
          var burstAddress = 0

          override def setupNextTransfer() = {
            if (remainingBurst == 0) {
              val burst = simRandom.nextFloat() > 0.5
              val lock = !burst & simRandom.nextFloat() > 0.5
              val size = simRandom.between(0, log2Up(ahb.HADDR.getWidth))
              val addressIncrement = 1 << size

              if (burst) {
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
              val maxAddr = ahb.HADDR.maxValue - (burstSize + 1) * addressIncrement
              burstAddress = simRandom.between(0, (maxAddr + 1).toInt) & ~(1 << size)
              ahb.HADDR #= burstAddress
              ahb.HMASTLOCK #= lock
              ahb.HSIZE #= size

            } else {
              remainingBurst = remainingBurst - 1
              ahb.HTRANS #= HTRANS.SEQ
              ahb.HADDR #= nextBurstAddress(ahb.HADDR.toBigInt, ahb.HBURST.toInt, ahb.HSIZE.toInt)
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
        hreadyoutWhenIdle = Some(true)
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

  def nextBurstAddress(address: BigInt, hburst: Int, hsize: Int): BigInt = {
    simLogId("bbbbbbbbbbbb")("hsize", hsize, "hburst", hburst)
    val unwrappedNextAddress = address + (1 << hsize) / 8
    val wrapped = if ((hburst & 1) == 1) {
      unwrappedNextAddress
    } else {
      val offsetBits = hsize + (hburst >> 1) + 1
      val boundary = 1 << offsetBits

      val static = (address >> offsetBits) << offsetBits
      val dynamic = unwrappedNextAddress & (boundary - 1)
      static + dynamic
    }
    simLogId("bbbbbbbbbbbb")("next", wrapped)
    wrapped
  }
}
