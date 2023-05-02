package andreasWallner

import andreasWallner.sim.{ComponentSimStringPimper, SimString, simLog}
import org.scalatest.Ignore
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite._

import scala.util.Random

case class AhbLite3Control(address: BigInt, write: Boolean, size: Int, prot: Int, trans: Int) {
  override def toString() = {
    val action = if (write) "W" else "R"
    f"($address%04x $action)"
  }
}

abstract case class AhbLite3SlaveAgent(ahb: AhbLite3, cd: ClockDomain, ss: SimString) {
  var delay = 0
  var addressPhase: Option[AhbLite3Control] = None

  cd.onSamplings {
    ahb.HRDATA.randomize()
    ahb.HRESP.randomize()

    if (addressPhase.isDefined)
      delay = 0.max(delay - 1)

    if (addressPhase.isDefined && delay == 0) {
      val ap = addressPhase.get
      if (ap.write) {
        val resp = onWrite(ap.address, ahb.HWDATA.toBigInt)
        ahb.HRESP #= resp
      } else {
        val (value, resp) = onRead(ap.address)
        ahb.HRDATA #= value
        ahb.HRESP #= resp
      }
      addressPhase = None
    }
    // TODO verify that HWRITE stays constant during data phase
    // TODO can HREADYOUT be low before a transfer starts?? model does not do that currently
    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean && delay == 0) {
      addressPhase = Some(
        AhbLite3Control(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
      delay = nextDelay()
    }
    if (delay > 0 && addressPhase.isDefined) {
      ahb.HREADYOUT #= false
    } else {
      ahb.HREADYOUT #= true
    }
    ss #= s"$delay $addressPhase"
  }
  def nextDelay(): Int = Random.nextInt(3)
  def onRead(address: BigInt): (BigInt, Boolean)
  def onWrite(address: BigInt, value: BigInt): Boolean
}

abstract case class AhbLite3SlaveMonitor(ahb: AhbLite3, cd: ClockDomain) {
  var addressPhase: Option[AhbLite3Control] = None
  cd.onSamplings {
    if (addressPhase.isDefined && ahb.HREADY.toBoolean) {
      val ap = addressPhase.get
      if (ap.write) {
        onWrite(ap.address, ahb.HWDATA.toBigInt)
      } else {
        onRead(ap.address, ahb.HRDATA.toBigInt)
      }
      addressPhase = None
    }

    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean) {
      addressPhase = Some(
        AhbLite3Control(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}

// TODO handle early error response
// TODO add parameter to simulate interconnect (which may change control during "address phase"
abstract case class AhbLite3MasterAgent(ahb: AhbLite3Master, cd: ClockDomain, ss: SimString) {
  def setupNextTransfer(): Option[BigInt]
  def nextDelay(): Int = Random.nextInt(3)

  ahb.HTRANS #= 0 // IDLE
  ahb.HBURST #= 0
  var delay = nextDelay()
  var nextHWDATA: Option[BigInt] = None
  var inAddressPhase = false
  var inDataPhase = false
  cd.onSamplings {
    delay = 0.max(delay - 1)

    if(inDataPhase && ahb.HREADY.toBoolean)
      inDataPhase = false

    if (inAddressPhase && !inDataPhase) {
      // TODO fix error here: address phase is always only a single cycle,
      // watching HREADY for address phase is only a slave thing -> master
      // must not change
      // - 3.1 states that address may be extended by previous bus transfer
      // - 3.6.1 when transfer is started HTRANS must stay constant
      // - 3.6.2 once HTRANS is nonseq, address must stay constant
      // - 4.1 slave samples address & control when HSEL && HREADY -> interconnect may change prio during wait
      ahb.HTRANS #= 0 // IDLE
      if (nextHWDATA.isDefined)
        ahb.HWDATA #= nextHWDATA.get
      nextHWDATA = None

      inAddressPhase = false
      inDataPhase = true
    }

    if (delay == 0 && !inAddressPhase) {
      ahb.HTRANS #= 2 // NONSEQ
      ahb.HBURST #= 0
      ahb.HMASTLOCK #= false
      //ahb.HSIZE #= 2
      nextHWDATA = setupNextTransfer()
      //ahb.HWRITE #= nextHWRITE.isDefined
      delay = nextDelay()
      inAddressPhase = true
    } else if (!inAddressPhase) {
      ahb.HWRITE.randomize()
      ahb.HADDR.randomize()
      ahb.HSIZE.randomize()
      ahb.HBURST.randomize()
      ahb.HMASTLOCK.randomize()
      ahb.HPROT.randomize()
      //delay = 0.max(delay - 1)
    }

    if (!inDataPhase) { // TODO no need to keep stable through read (note in 3.1)
      ahb.HWRITE.randomize()
    }
    ss #= s"$delay $inAddressPhase $inDataPhase"
  }
}

abstract case class AhbLite3MasterMonitor(ahb: AhbLite3Master, cd: ClockDomain, idx: Int) {
  var addressPhase: Option[AhbLite3Control] = None
  cd.onSamplings {
    if (addressPhase.isDefined && ahb.HREADY.toBoolean) {
      val ap = addressPhase.get
      if (ap.write) {
        onWrite(ap.address, ahb.HWDATA.toBigInt)
      } else {
        onRead(ap.address, ahb.HRDATA.toBigInt)
      }
      addressPhase = None
    }

    if (ahb.HTRANS.toInt != 0 && addressPhase.isEmpty) {
      assert(ahb.HTRANS.toInt == 2, "only NONSEQ is currently supported")
      addressPhase = Some(
        AhbLite3Control(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
      simLog(idx, addressPhase)
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}

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

@Ignore
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
          Some(ahb.HWDATA.randomizedBigInt())
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
  val masterStrings = (0 until 2).map { i => SimString(s"master_$i") }
  val slaveStrings = (0 until 1).map { i => SimString(s"slave_$i") }
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
          Some(ahb.HWDATA.randomizedBigInt())
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
