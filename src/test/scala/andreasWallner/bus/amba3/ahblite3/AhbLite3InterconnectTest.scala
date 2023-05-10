package andreasWallner.bus.amba3.ahblite3

import andreasWallner.Utils.memoize
import andreasWallner._
import andreasWallner.bus.amba3.ahblite3.sim._
import andreasWallner.sim.SimString
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps

case class AhbInterconnectTester(
    masterCount: Int,
    slaveCount: Int,
    mappedSlaves: Seq[Seq[Int]],
    mappings: Seq[SizeMapping]
) extends Component {
  val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 4)
  val masterStrings = Seq.tabulate(masterCount) { i =>
    SimString(s"master_$i").materialize()
  }
  val slaveStrings = Seq.tabulate(slaveCount) { i =>
    SimString(s"slave_$i").materialize()
  }

  val io = new Bundle {
    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)), masterCount)
    val ahbSlaves = Vec(master(AhbLite3(ahbConfig)), slaveCount)
  }
  val matrix = AhbLite3Interconnect(masterCount, slaveCount, mappings)
  matrix.io.masters.zip(io.ahbMasters).foreach { case (x, y) => x <> y }
  matrix.io.slaves.zip(io.ahbSlaves).foreach { case (x, y)   => x <> y }

  def validAddress(master: Int) = {
    val slave = mappedSlaves(master).randomPick()
    mappings(slave).randomPick()
  }
  def slaveIndex(address: BigInt) =
    mappings.zipWithIndex.find { case (mapping, _) => mapping.hit(address) } match {
      case Some((_, idx)) => idx
      case _              => throw new Exception(f"No slave found for address $address%04x")
    }
}

class AhbLite3InterconnectTest extends SpinalFunSuite {
  case class TestParams(
      masterCount: Int,
      slaveCount: Int,
      connectivity: Seq[Seq[Int]],
      mappings: Seq[SizeMapping],
      slaveHreadyoutWhenIdle: Option[Boolean] = None
  ) {
    def name = {
      val connStr = connectivity.zipWithIndex
        .map { case (slaves, master) => f"""${master}_${slaves.mkString("")}""" }
        .mkString("_")
      val hreadyoutIdleString = slaveHreadyoutWhenIdle match {
        case None => "random"
        case Some(true) => "high"
        case Some(false) => "low"
      }
      f"${masterCount}M${slaveCount}S_${connStr}_${mappings.hashCode()}%x_$hreadyoutIdleString"
    }
  }

  lazy val dutFactory: TestParams => SimCompiled[AhbInterconnectTester] = memoize { params =>
    namedSimConfig.compile(
      AhbInterconnectTester(
        params.masterCount,
        params.slaveCount,
        params.connectivity,
        params.mappings
      ).setDefinitionName("AhbInterconnectTester_" + params.name).setName("AhbInterconnectTester")
    )
  }

  val toTest = Seq(
    TestParams(2, 1, Seq(Seq(0), Seq(0)), Seq(SizeMapping(0x1000, 0x1000))),
    TestParams(1, 3, Seq(Seq(0, 1, 2)), Seq.tabulate(3) { i =>
      SizeMapping(0x1000 * i, 0x1000)
    }),
    TestParams(2, 2, Seq.fill(2)(Seq(0, 1)), Seq.tabulate(2) { i =>
      SizeMapping(0x1000 * i, 0x1000)
    }),
    TestParams(2, 2, Seq.fill(2)(Seq(0, 1)), Seq.tabulate(2) { i =>
      SizeMapping(0x1000 * i, 0x1000)
    }, Some(true)),
    TestParams(2, 2, Seq.fill(2)(Seq(0, 1)), Seq.tabulate(2) { i =>
      SizeMapping(0x1000 * i, 0x1000)
    }, Some(false)),
    TestParams(3, 5, Seq.fill(3)(Seq(0, 1, 2, 3, 4)), Seq.tabulate(5) { i =>
      SizeMapping(0x1000 * i, 0x1000)
    })
  )
  for (params <- toTest) {
    test(dutFactory(params), "randomized " + params.name) { dut =>
      SimTimeout(10000 * 10)
      val masterTransactions = mutable.ArrayBuffer.fill(params.masterCount)(0)
      val slaveTransactions = mutable.ArrayBuffer.fill(params.slaveCount)(0)
      val scoreboards = IndexedSeq.tabulate(params.slaveCount) { i =>
        LoggingScoreboardInOrder[(BigInt, String, BigInt)](
          s"sb$i",
          (x: (BigInt, String, BigInt)) => f"${x._2}: 0x${x._1}%04x = 0x${x._3}%02x"
        )
      }

      for (i <- 0 until params.masterCount) {
        new AhbLite3MasterAgent(dut.io.ahbMasters(i), dut.clockDomain, dut.masterStrings(i)) {
          override def setupNextTransfer() = {
            ahb.HADDR #= dut.validAddress(i)
            Some(ahb.HWDATA.randomizedBigInt())
          }
        }
        new AhbLite3MasterMonitor(dut.io.ahbMasters(i), dut.clockDomain, i) {
          override def onRead(address: BigInt, value: BigInt): Unit = {
            //simLog(f">>$idx R $address%04x = $value%02x")
            val slave = dut.slaveIndex(address)
            scoreboards(slave).pushRef((address - dut.mappings(slave).base, "R", value))
            masterTransactions(i) = masterTransactions(i) + 1
          }

          override def onWrite(address: BigInt, value: BigInt): Unit = {
            //simLog(f">>$idx W $address%04x = $value%02x")
            val slave = dut.slaveIndex(address)
            scoreboards(slave).pushRef((address - dut.mappings(slave).base, "W", value))
            masterTransactions(i) = masterTransactions(i) + 1
          }
        }
      }

      val readData = Seq.fill(params.slaveCount)(mutable.ArrayBuffer[Int]())
      for (i <- 0 until params.slaveCount) {
        new AhbLite3SlaveAgent(dut.io.ahbSlaves(i), dut.clockDomain, dut.slaveStrings(i), params.slaveHreadyoutWhenIdle) {
          var readCount = 0
          override def onRead(address: BigInt) = {
            readCount = (readCount % 16)+ 1
            (readCount - 1/*ahb.HRDATA.randomizedBigInt()*/, false)
          }

          override def onWrite(address: BigInt, value: BigInt) = false
        }

        new AhbLite3SlaveMonitor(dut.io.ahbSlaves(i), dut.clockDomain) {
          override def onRead(address: BigInt, value: BigInt): Unit = {
            scoreboards(i).pushDut((address, "R", value))
            readData(i).append(value.toInt)
            slaveTransactions(i) = slaveTransactions(i) + 1
          }

          override def onWrite(address: BigInt, value: BigInt): Unit = {
            scoreboards(i).pushDut((address, "W", value))
            slaveTransactions(i) = slaveTransactions(i) + 1
          }
        }
      }
      dut.clockDomain.forkStimulus(10)
      waitUntil(
        masterTransactions.min > 50 * params.slaveCount && slaveTransactions.min > 50 * params.slaveCount
      )
      readData.map { d =>
        d.zipWithIndex.map{case (value, idx) => assert(value == idx % 16, f"mismatch $value ${idx%16} ($idx)")}
      }
      println(masterTransactions.mkString(" "), slaveTransactions.mkString(" "))
    }
  }
}
