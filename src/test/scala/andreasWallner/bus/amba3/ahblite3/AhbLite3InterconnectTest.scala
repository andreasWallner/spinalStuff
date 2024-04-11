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

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.language.postfixOps

trait TestParams {
  def name: String
  val masterCount: Int
  def slaveCount: Int
  def mappings: Seq[SizeMapping]
  val slaveHreadyoutWhenIdle: Option[Boolean]
}

case class FullyConn(
    masterCount: Int,
    slaveCount: Int,
    inMappings: Seq[SizeMapping] = null,
    slaveHreadyoutWhenIdle: Option[Boolean] = None
) extends TestParams {
  def mappings: Seq[SizeMapping] = inMappings match {
    case null =>
      Seq.tabulate(slaveCount) { i =>
        SizeMapping(0x1000 * i, 0x1000)
      }
    case _ => inMappings
  }
  def name = {
    val hreadyoutIdleString = slaveHreadyoutWhenIdle match {
      case None        => "random"
      case Some(true)  => "high"
      case Some(false) => "low"
    }
    f"${masterCount}M${slaveCount}S_full_${mappings.hashCode()}%x_$hreadyoutIdleString"
  }
}

case class Sparse(
    connectivity: Seq[Seq[Int]],
    inMappings: Seq[SizeMapping] = null,
    slaveHreadyoutWhenIdle: Option[Boolean] = None
) extends TestParams {
  def name = {
    val connStr = connectivity.zipWithIndex
      .map { case (slaves, master) => f"""${master}_${slaves.mkString("")}""" }
      .mkString("_")
    val hreadyoutIdleString = slaveHreadyoutWhenIdle match {
      case None        => "random"
      case Some(true)  => "high"
      case Some(false) => "low"
    }
    f"${masterCount}M${slaveCount}S_sparse_${connStr}_${mappings.hashCode()}%x_$hreadyoutIdleString"
  }
  def mappings = inMappings match {
    case null =>
      Seq.tabulate(slaveCount) { i =>
        SizeMapping(0x1000 * i, 0x1000)
      }
    case _ => inMappings
  }
  def slaveConnectivity = AhbLite3Interconnect.calcSlaveConnectivity(connectivity)
  val masterCount = connectivity.size
  val slaveCount = slaveConnectivity.size
}

object AhbInterconnectTester {
  def apply(tp: TestParams) = tp match {
    case fc: FullyConn =>
      new AhbInterconnectTester(
        fc.mappings,
        Seq.fill(fc.masterCount)(Seq.tabulate(fc.slaveCount)(i => i))
      )
    case sc: Sparse =>
      new AhbInterconnectTester(sc.mappings, sc.connectivity)
  }
}

class AhbInterconnectTester(
    decodings: Seq[SizeMapping],
    masterConnectivity: Seq[Seq[Int]]
) extends Component {
  val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 4)

  val slaveConnectivity = AhbLite3Interconnect.calcSlaveConnectivity(masterConnectivity)
  val masterCount = masterConnectivity.size
  val slaveCount = slaveConnectivity.size
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
  val matrix = AhbLite3Interconnect(decodings, masterConnectivity, ahbConfig)
  matrix.io.masters.zip(io.ahbMasters).foreach { case (x, y) => x <> y }
  matrix.io.slaves.zip(io.ahbSlaves).foreach { case (x, y)   => x <> y }

  def validAddress(master: Int) = {
    val slave = masterConnectivity(master).randomPick()
    decodings(slave).randomPick()
  }
  def slaveIndex(address: BigInt) =
    decodings.zipWithIndex.find { case (mapping, _) => mapping.hit(address) } match {
      case Some((_, idx)) => idx
      case _              => throw new Exception(f"No slave found for address $address%04x")
    }
}

class AhbLite3InterconnectTest extends SpinalFunSuite {
  lazy val dutFactory: TestParams => SimCompiled[AhbInterconnectTester] = memoize { params =>
    namedSimConfig.compile(
      AhbInterconnectTester(params)
        .setDefinitionName("AhbInterconnectTester_" + params.name)
        .setName("AhbInterconnectTester")
    )
  }

  val toTest = Seq(
    FullyConn(2, 1),
    FullyConn(1, 3),
    FullyConn(2, 2),
    FullyConn(2, 2, slaveHreadyoutWhenIdle = Some(true)),
    FullyConn(2, 2, slaveHreadyoutWhenIdle = Some(false)),
    FullyConn(3, 5),
    Sparse(Seq(Seq(0, 1), Seq(1, 2), Seq(3))),
    Sparse(Seq(Seq(0), Seq(1), Seq(2)))
  )
  for (params <- toTest) {
    test(dutFactory(params), "randomized " + params.name) { dut =>
      SimTimeout(10000 * 10)
      val masterTransactions = mutable.ArrayBuffer.fill(params.masterCount)(0)
      val slaveTransactions = mutable.ArrayBuffer.fill(params.slaveCount)(0)
      val scoreboards = IndexedSeq.tabulate(params.slaveCount) { i =>
        LoggingScoreboardInOrder[(BigInt, String, BigInt)](
          enable = false,
          s"sb$i",
          (x: (BigInt, String, BigInt)) => f"${x._2}: 0x${x._1}%04x = 0x${x._3}%02x"
        )
      }

      for (i <- 0 until params.masterCount) {
        new AhbLite3MasterAgent(dut.io.ahbMasters(i), dut.clockDomain, dut.masterStrings(i)) {
          override def setupNextTransfer() = {
            ahb.HADDR #= dut.validAddress(i)
            (Some(ahb.HWDATA.randomizedBigInt()), false)
          }
        }
        new AhbLite3MasterMonitor(dut.io.ahbMasters(i), dut.clockDomain, i) {
          override def onRead(address: BigInt, value: BigInt): Unit = {
            //simLog(f">>$idx R $address%04x = $value%02x")
            val slave = dut.slaveIndex(address)
            scoreboards(slave).pushRef((address, "R", value))
            masterTransactions(i) = masterTransactions(i) + 1
          }

          override def onWrite(address: BigInt, value: BigInt): Unit = {
            //simLog(f">>$idx W $address%04x = $value%02x")
            val slave = dut.slaveIndex(address)
            scoreboards(slave).pushRef((address, "W", value))
            masterTransactions(i) = masterTransactions(i) + 1
          }
        }
      }

      for (i <- 0 until params.slaveCount) {
        new AhbLite3SlaveAgent(
          dut.io.ahbSlaves(i),
          dut.clockDomain,
          dut.slaveStrings(i),
          params.slaveHreadyoutWhenIdle
        ) {
          override def onRead(address: BigInt) = (ahb.HRDATA.randomizedBigInt(), false)

          override def onWrite(address: BigInt, value: BigInt) = false
        }

        new AhbLite3SlaveMonitor(dut.io.ahbSlaves(i), dut.clockDomain) {
          override def onRead(address: BigInt, value: BigInt): Unit = {
            scoreboards(i).pushDut((params.mappings(i).base + address, "R", value))
            slaveTransactions(i) = slaveTransactions(i) + 1
          }

          override def onWrite(address: BigInt, value: BigInt): Unit = {
            scoreboards(i).pushDut((params.mappings(i).base + address, "W", value))
            slaveTransactions(i) = slaveTransactions(i) + 1
          }
        }
        AhbLite3ProtocolChecker(dut.io.ahbSlaves(i), dut.clockDomain, s"S$i")
      }
      dut.clockDomain.forkStimulus(10)
      waitUntil(
        masterTransactions.min > 50 * params.slaveCount && slaveTransactions.min > 50 * params.slaveCount
      )
      println(masterTransactions.mkString(" "), slaveTransactions.mkString(" "))
    }
  }
}
