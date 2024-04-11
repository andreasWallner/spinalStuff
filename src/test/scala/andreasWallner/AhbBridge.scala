package andreasWallner

import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.{ComponentSimStringPimper, SimString, simLog}
import org.scalatest.Ignore
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class Tester extends Component {
  val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 4)

  val io = new Bundle {
    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)), 3)
    val ahbSlaves = Vec(master(AhbLite3(ahbConfig)), 4)
  }

  val crossbar = AhbLite3CrossbarFactory(ahbConfig)
    .addSlaves(
      io.ahbSlaves(0) -> (0x1000, 0x1000),
      io.ahbSlaves(1) -> (0x3000, 0x1000),
      io.ahbSlaves(2) -> (0x4000, 0x1000),
      io.ahbSlaves(3) -> (0x5000, 0x1000)
    )
    .addConnections(
      io.ahbMasters(0).toAhbLite3() -> List(io.ahbSlaves(0), io.ahbSlaves(1)),
      io.ahbMasters(1).toAhbLite3() -> List(io.ahbSlaves(1), io.ahbSlaves(2), io.ahbSlaves(3)),
      io.ahbMasters(2).toAhbLite3() -> List(io.ahbSlaves(0), io.ahbSlaves(3))
    )
    // ** OPTIONAL **
    //.addGlobalDefaultSlave(io.defaultSalve)
    //.addDefaultSalves(
    //   io.ahbMaster(0) -> io.defaultSlaveM0,
    //   io.ahbMaster(1) -> io.defaultSalveM1
    //)
    .build()
}

class AhbBridgeTest extends SpinalFunSuite {
  val masterStrings = (0 until 3).map { i =>
    SimString(s"master_$i")
  }
  val slaveStrings = (0 until 4).map { i =>
    SimString(s"slave_$i")
  }
  val dut = namedSimConfig.compile(new Tester().add(masterStrings).add(slaveStrings))
  test(dut, "blub", seed = 2) { dut =>
    SimTimeout(10000)
    val scoreboards = (0 until 4).map { i =>
      LoggingScoreboardInOrder[(Int, BigInt, String, BigInt)](
        s"sb$i",
        (x: (Int, BigInt, String, BigInt)) => f"${x._1} ${x._3}: 0x${x._2}%04x = 0x${x._4}%02x"
      )
    }

    def validAddress(slaves: Int*) = {
      Random.nextInt(0x1000) + (/*slaves.randomPick()*/ 0 match {
        case 0 => 0x1000
        case 1 => 0x3000
        case 2 => 0x4000
        case 3 => 0x5000
      })
    }

    def slaveIndex(address: BigInt): Int =
      address match {
        case _ if (0x1000 until 0x2000).contains(address) => 0
        case _ if (0x3000 until 0x4000).contains(address) => 1
        case _ if (0x4000 until 0x5000).contains(address) => 2
        case _ if (0x5000 until 0x6000).contains(address) => 3
        case _ =>
          throw new Exception(f"invalid address $address%08x")
      }

    val mappedSlaves = Seq(Seq(0, 1), Seq(1, 2, 3), Seq(0, 3))

    for (i <- 0 until 3) {
      new AhbLite3MasterAgent(dut.io.ahbMasters(i), dut.clockDomain, masterStrings(i)) {
        override def setupNextTransfer() = {
          ahb.HADDR #= validAddress(mappedSlaves(i): _*)
          (Some(ahb.HWDATA.randomizedBigInt()), false)
        }

      }
      new AhbLite3MasterMonitor(dut.io.ahbMasters(i), dut.clockDomain, i) {
        override def onRead(address: BigInt, value: BigInt): Unit = {
          //simLog(f">>$i R $address%04x = $value%02x")
          scoreboards(slaveIndex(address)).pushRef((slaveIndex(address), address, "R", value))
        }

        override def onWrite(address: BigInt, value: BigInt): Unit = {
          //simLog(f">>$i W $address%04x = $value%02x")
          scoreboards(slaveIndex(address)).pushRef((slaveIndex(address), address, "W", value))
        }
      }
    }

    for (i <- 0 until 4) {
      new AhbLite3SlaveAgent(dut.io.ahbSlaves(i), dut.clockDomain, slaveStrings(i)) {
        override def onRead(address: BigInt) = (ahb.HRDATA.randomizedBigInt(), false)

        override def onWrite(address: BigInt, value: BigInt) = false
      }
      new AhbLite3SlaveMonitor(dut.io.ahbSlaves(i), dut.clockDomain) {
        override def onRead(address: BigInt, value: BigInt): Unit =
          scoreboards(i).pushDut((i, address, "R", value))

        override def onWrite(address: BigInt, value: BigInt): Unit =
          scoreboards(i).pushDut((i, address, "W", value))
      }
    }

    dut.clockDomain.forkStimulus(10)
    waitUntil(scoreboards.last.matches == 10)
  }
}

class TesterM2S1 extends Component {
  val ahbConfig = AhbLite3Config(addressWidth = 4, dataWidth = 4)

  val io = new Bundle {
    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)), 2)
    val ahbSlaves = Vec(master(AhbLite3(ahbConfig)), 1)
  }

  val crossbar = AhbLite3CrossbarFactory(ahbConfig)
    .addSlaves(
      io.ahbSlaves(0) -> (0, 0x10)
    )
    .addConnections(
      io.ahbMasters(0).toAhbLite3() -> List(io.ahbSlaves(0)),
      io.ahbMasters(1).toAhbLite3() -> List(io.ahbSlaves(0))
    )
    // ** OPTIONAL **
    //.addGlobalDefaultSlave(io.defaultSalve)
    //.addDefaultSalves(
    //   io.ahbMaster(0) -> io.defaultSlaveM0,
    //   io.ahbMaster(1) -> io.defaultSalveM1
    //)
    .build()
}

class AhbBridgeTest2M1S extends SpinalFunSuite {
  val masterStrings = Seq.tabulate(2) { i =>
    SimString(s"master_$i")
  }
  val slaveStrings = Seq.tabulate(1) { i =>
    SimString(s"slave_$i")
  }
  val dut = namedSimConfig.compile(new TesterM2S1().add(masterStrings).add(slaveStrings))

  test(dut, "randomized", seed = 2) { dut =>
    SimTimeout(10000)
    val scoreboards = (0 until 4).map { i =>
      LoggingScoreboardInOrder[(Int, BigInt, String, BigInt)](
        s"sb$i",
        (x: (Int, BigInt, String, BigInt)) => f"${x._1} ${x._3}: 0x${x._2}%04x = 0x${x._4}%02x"
      )
    }

    def validAddress(slaves: Int*) = Random.nextInt(0x10)
    def slaveIndex(address: BigInt): Int = 0
    val mappedSlaves = Seq(Seq(0), Seq(0))

    for (i <- 0 until 2) {
      new AhbLite3MasterAgent(dut.io.ahbMasters(i), dut.clockDomain, masterStrings(i)) {
        override def setupNextTransfer() = {
          ahb.HADDR #= validAddress(mappedSlaves(i): _*)
          (Some(ahb.HWDATA.randomizedBigInt()), false)
        }
      }
      new AhbLite3MasterMonitor(dut.io.ahbMasters(i), dut.clockDomain, i) {
        override def onRead(address: BigInt, value: BigInt): Unit = {
          //simLog(f">>$idx R $address%04x = $value%02x")
          scoreboards(slaveIndex(address)).pushRef((slaveIndex(address), address, "R", value))
        }

        override def onWrite(address: BigInt, value: BigInt): Unit = {
          //simLog(f">>$idx W $address%04x = $value%02x")
          scoreboards(slaveIndex(address)).pushRef((slaveIndex(address), address, "W", value))
        }
      }
    }

    for (i <- 0 until 1) {
      new AhbLite3SlaveAgent(dut.io.ahbSlaves(i), dut.clockDomain, slaveStrings(i)) {
        override def onRead(address: BigInt) = (ahb.HRDATA.randomizedBigInt(), false)
        override def onWrite(address: BigInt, value: BigInt) = false
      }

      new AhbLite3SlaveMonitor(dut.io.ahbSlaves(i), dut.clockDomain) {
        override def onRead(address: BigInt, value: BigInt): Unit =
          scoreboards(i).pushDut((i, address, "R", value))
        override def onWrite(address: BigInt, value: BigInt): Unit =
          scoreboards(i).pushDut((i, address, "W", value))
      }
    }
    dut.clockDomain.forkStimulus(10)
    waitUntil(scoreboards.last.matches == 10)
  }
}


