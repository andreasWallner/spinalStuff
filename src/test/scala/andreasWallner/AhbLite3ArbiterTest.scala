package andreasWallner

import andreasWallner.Utils.memoize
import andreasWallner.bus.amba3.ahblite3.AhbLite3Arbiter
import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.{SimString, simLog, simLogId}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.sim.ScoreboardInOrder

import scala.collection.mutable

class AhbBurstGen(val ahb: AhbLite3Master) {
  var burstSize = 0
  var remainingBurst = 0
  var burstType = 0
  var burstAddress = 0

  def setupTransfer() = {
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

  def nextBurstAddress(address: BigInt, hburst: Int, hsize: Int): BigInt = {
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
    wrapped
  }
}

class AhbLite3ArbiterTest extends SpinalFunSuite {
  for ((roundRobinArbiter, masterDelay, slaveDelay) <- Seq(
         (false, 3, 3),
         (false, 3, 0),
    (false, 8, 0),
         (true, 3, 3),
         (true, 3, 0),
         (true, 0, 3),
         (true, 0, 0),
       )) {
    val dutName = if (roundRobinArbiter) "roundRobin" else "prio"
    val testPostfix = dutName + s"_m${masterDelay}_s${slaveDelay}"

    lazy val dutFactory = memoize { rr: Boolean =>
      namedSimConfig.extendName(dutName).compile {
        val config = AhbLite3Config(8, 16)
        val masterCnt = 2
        new Component {
          val io = new Bundle {
            val inputs = Vec(slave port AhbLite3Master(config), masterCnt)
            val output = master port AhbLite3(config)
          }
          val inner = AhbLite3Arbiter(config, masterCnt, rr)
          for (i <- 0 until masterCnt) {
            io.inputs(i).toAhbLite3() >> inner.io.inputs(i)
          }
          inner.io.output >> io.output
        }
      }
    }

    test(dutFactory(roundRobinArbiter), s"bursts_$testPostfix") { dut =>
      SimTimeout(200000)

      val scoreboard = ScoreboardInOrder[(BigInt, String, BigInt)]()
      val masterCounts = mutable.Seq.fill(2)(0)

      for (i <- 0 until 2) {
        val burstGen = new AhbBurstGen(dut.io.inputs(i))
        new AhbLite3MasterAgent(dut.io.inputs(i), dut.clockDomain) {
          override def nextDelay() = if (masterDelay != 0) simRandom.nextInt(masterDelay) else 0
          override def setupNextTransfer() = burstGen.setupTransfer()
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
        hreadyoutWhenIdle = None
      ) {
        override def nextDelay() = if (slaveDelay != 0) simRandom.nextInt(slaveDelay) else 0
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

    test(dutFactory(roundRobinArbiter), s"simple_$testPostfix") { dut =>
      SimTimeout(200000)

      val scoreboard = ScoreboardInOrder[(BigInt, String, BigInt)]()
      val masterCounts = mutable.Seq.fill(2)(0)
      for (i <- 0 until 2) {
        val masterScoreboard = ScoreboardInOrder[(BigInt, String, BigInt)]()
        val burstGen = new AhbBurstGen(dut.io.inputs(i))
        new AhbLite3MasterAgent(dut.io.inputs(i), dut.clockDomain) {
          override def nextDelay() = if (masterDelay != 0) simRandom.nextInt(masterDelay) else 0
          override def setupNextTransfer() = {
            val next = (ahb.HWDATA.randomizedBigInt() & 0xfff0) + i
            masterScoreboard.pushRef((0, "W", next))
            (Some(next), false)
          }
        }
        new AhbLite3MasterMonitor(dut.io.inputs(i), dut.clockDomain, i) {
          override def onRead(address: BigInt, value: BigInt): Unit = {
            scoreboard.pushRef((address, "R", value))
            masterCounts(i) = masterCounts(i) + 1
          }

          override def onWrite(address: BigInt, value: BigInt): Unit = {
            scoreboard.pushRef((address, "W", value))
            masterScoreboard.pushRef((0, "W", value))
            masterCounts(i) = masterCounts(i) + 1
          }
        }
        AhbLite3ProtocolChecker(dut.io.inputs(i), dut.clockDomain, f"M$i")
      }

      new AhbLite3SlaveAgent(
        dut.io.output,
        dut.clockDomain,
        hreadyoutWhenIdle = None
      ) {
        override def nextDelay() = if (slaveDelay != 0) simRandom.nextInt(slaveDelay) else 0
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
}

object AhbLite3ArbiterGen extends App {
  SpinalVerilog {
    val config = AhbLite3Config(8, 16)
    AhbLite3Arbiter(config, 2)
  }.printPruned()
}
