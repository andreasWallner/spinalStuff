package andreasWallner.ztex

import andreasWallner.SpinalFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.sim.{
  ScoreboardInOrder,
  StreamDriver,
  StreamMonitor,
  StreamReadyRandomizer
}

import scala.collection.mutable
import scala.util.Random

abstract case class Apb3Monitor(apb: Apb3, clockDomain: ClockDomain) {
  sealed trait State

  case object Idle extends State

  case object Setup extends State

  case object Access extends State

  var state: State = Idle
  var lastState = state
  var ready_delay = 0

  var paddr: BigInt = 0
  var pwrite = false
  var pwdata: BigInt = 0
  clockDomain.onSamplings {
    lastState = state
    state = state match {
      case Idle =>
        if (apb.PSEL.toInt == 1) {
          paddr = apb.PADDR.toBigInt
          pwdata = apb.PWDATA.toBigInt
          pwrite = apb.PWRITE.toBoolean
          ready_delay = nextDelay()
          Setup
        } else {
          Idle
        }
      case Setup | Access =>
        if (apb.PREADY.toBoolean)
          Idle
        else
          Access
    }

    state match {
      case Idle =>
        // no requirements on PREADY until PENABLE (Figure 2-1 T1)
        // no requirements on PSLVERROR until PREADY (Figure 2-6 T4)
        apb.PREADY.randomize()
        if (apb.config.useSlaveError)
          apb.PSLVERROR.randomize()

        if (lastState == Idle && apb.PENABLE.toBoolean) {
          onProtocolError("PENABLE must stay at False during bus IDLE")
        }
      case Setup =>
        checkMasterSignals()

        apb.PREADY #= false
        if (apb.config.useSlaveError)
          apb.PSLVERROR #= false
        update()
      case Access =>
        checkMasterSignals()
        if (!apb.PENABLE.toBoolean)
          onProtocolError("PENABLE must be set during Access/Setup state")
        update()
    }
  }

  def checkMasterSignals(): Unit = {
    if (apb.PADDR.toBigInt != paddr || apb.PWDATA.toBigInt != pwdata || apb.PWRITE.toBoolean != pwrite)
      onProtocolError(
        f"PADDR, PWDATA & PWRITE must not change after selection ($paddr, $pwdata, $pwrite)"
      )
  }

  def update(): Unit = {
    val address = apb.PADDR.toBigInt
    if (ready_delay == 0) {
      apb.PREADY #= true
      if (!apb.PWRITE.toBoolean) {
        val bytes = new Array[Byte](apb.config.dataWidth / 8 + 1)
        for (i <- 0 until bytes.length - 1) {
          bytes(i + 1) = onRead(address + apb.config.dataWidth / 8 - 1 - i)
        }
        apb.PRDATA #= BigInt(bytes)
      } else {
        val bytes = apb.PWDATA.toBigInt.toByteArray.reverse
        for (i <- 0 until apb.config.dataWidth / 8) {
          onWrite(address + i, if (bytes.length > i) bytes(i) else 0)
        }
      }
    }
    if (ready_delay > 0) {
      ready_delay -= 1
    }
  }

  def onProtocolError(text: String): Unit
  def nextDelay() = Random.nextInt(5)
  def onRead(address: BigInt): Byte
  def onWrite(address: BigInt, value: Byte): Unit
}

class BusMasterSim extends SpinalFunSuite {
  def addTestWrite(
                    queue: mutable.Queue[Int],
                    scoreboard: ScoreboardInOrder[(BigInt, Int)]
                  ): Unit = {
    // make address that is 4-byte aligned and selects the first slave (first 4 bit = 0)
    val address = Random.nextInt(0x1000) & 0xFFFC
    val data = for (_ <- 0 to 3) yield Random.nextInt(256)
    queue ++= Array(
      0x0100,
      address,
      (data(0) << 8) + data(1),
      (data(2) << 8) + data(3)
    )
    for (i <- 0 to 3)
      scoreboard.pushRef((BigInt(address + i), data(3 - i)))
  }

  def byteAtAddress(address: BigInt) =
    address.toByteArray.reduce((a, b) => (a ^ b).toByte)

  def addTestRead(
                   queue: mutable.Queue[Int],
                   scoreboard: ScoreboardInOrder[(Int, BigInt)]
                 ): Unit = {
    val source = Random.nextInt(0x100)
    val address = Random.nextInt(0x1000) & 0xFFFC
    queue ++= Array(
      0x0200 + source,
      address
    )
    val b0 = BigInt(byteAtAddress(address) & 0xff)
    val b1 = BigInt(byteAtAddress(address + 1) & 0xff)
    val b2 = BigInt(byteAtAddress(address + 2) & 0xff)
    val b3 = BigInt(byteAtAddress(address + 3) & 0xff)
    val data = (b3 << 24) + (b2 << 16) + (b1 << 8) + b0
    scoreboard.pushRef((source, data))
  }

  val dut = SimConfig.withWave
    .compile(BusMaster())

  test(dut, "write registers") { dut =>
    val toWrite = 20
    SimTimeout(toWrite * 40 * 10)

    val q = mutable.Queue[Int]()
    val scoreboard = ScoreboardInOrder[(BigInt, Int)]()
    for (_ <- 0 until toWrite)
      addTestWrite(q, scoreboard)

    StreamDriver(dut.io.data, dut.clockDomain) { payload =>
      val nonEmpty = q.nonEmpty
      if (nonEmpty) payload #= q.dequeue()
      nonEmpty
    }
    new Apb3Monitor(dut.io.apb3, dut.clockDomain) {
      def onRead(address: BigInt): Byte = {
        fail("unexpected read")
        0
      }

      def onWrite(address: BigInt, value: Byte): Unit =
        scoreboard.pushDut((address, value & 0xff))

      def onProtocolError(text: String): Unit = fail(text)
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitActiveEdgeWhere(scoreboard.ref.isEmpty)
    dut.clockDomain.waitActiveEdge(10)
    scoreboard.checkEmptyness()
  }

  test(dut, "write registers, full backpressure") { dut =>
    val toWrite = 20
    SimTimeout(toWrite * 10 * 10)

    val q = mutable.Queue[Int]()
    val scoreboard = ScoreboardInOrder[(BigInt, Int)]()
    for (_ <- 0 until toWrite)
      addTestWrite(q, scoreboard)

    val driver = StreamDriver(dut.io.data, dut.clockDomain) { payload =>
      val nonEmpty = q.nonEmpty
      if (nonEmpty) payload #= q.dequeue()
      nonEmpty
    }
    driver.transactionDelay = () => 0
    new Apb3Monitor(dut.io.apb3, dut.clockDomain) {
      def onRead(address: BigInt): Byte = {
        fail("unexpected read")
        0
      }

      def onWrite(address: BigInt, value: Byte): Unit = {
        scoreboard.pushDut((address, value & 0xff))
      }

      def onProtocolError(text: String): Unit = {
        fail(text)
      }
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitActiveEdgeWhere(scoreboard.ref.isEmpty)
    dut.clockDomain.waitActiveEdge(10)
    scoreboard.checkEmptyness()
  }

  test(dut, "read registers", seed = 956489631) { dut =>
    val toRead = 2
    SimTimeout(toRead * 30 * 11)

    val q = mutable.Queue[Int]()
    val scoreboard = ScoreboardInOrder[(Int, BigInt)]()
    for (_ <- 0 until toRead)
      addTestRead(q, scoreboard)

    StreamDriver(dut.io.data, dut.clockDomain) { payload =>
      val nonEmpty = q.nonEmpty
      if (nonEmpty) payload #= q.dequeue()
      nonEmpty
    }
    StreamReadyRandomizer(dut.io.resp, dut.clockDomain)
    val tempQueue = mutable.Queue[Int]()
    StreamMonitor(dut.io.resp, dut.clockDomain) { payload =>
      tempQueue += payload.toInt
      if (tempQueue.size == 3) {
        tempQueue.head >> 8 match {
          case 0x02 =>
            val access = (
              tempQueue.head & 0xff,
              (BigInt(tempQueue(1)) << 16) + BigInt(tempQueue(2))
            )
            println(f"${access._1}%x ${access._2}%x")
            scoreboard.pushDut(access)
          case _ => fail("invalid response code sent")
        }
        tempQueue.clear()
      }
    }
    new Apb3Monitor(dut.io.apb3, dut.clockDomain) {
      def onRead(address: BigInt): Byte = byteAtAddress(address)

      def onWrite(address: BigInt, value: Byte): Unit = fail("unexpected write")

      def onProtocolError(text: String): Unit = fail(text)
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitActiveEdgeWhere(scoreboard.ref.isEmpty)
    dut.clockDomain.waitActiveEdge(10)
    scoreboard.checkEmptyness()
  }
}
