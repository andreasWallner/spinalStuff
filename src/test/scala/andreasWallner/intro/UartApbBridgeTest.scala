package andreasWallner.intro

import andreasWallner.Utils.{evenParity, oddParity}
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import andreasWallner.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.sim.SimManagerContext

import org.scalactic.TimesOnInt.convertIntToRepeater
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class UartSimDriver(
    pin: Bool,
    baudrate: HertzNumber,
    parity: String = "odd",
    stopBits: Double = 2.0,
    timeScale: TimeNumber = 1 ps
) {
  val baudPeriod = (baudrate.toTime / timeScale).toLong
  assert(baudPeriod > 0, "can't simulate uart datarate with given simulator timescale")
  pin #= true

  def send(b: Byte): Unit = {
    pin #= false
    sleep(baudPeriod)
    for (i <- 0 to 7) {
      pin #= ((b >> i) & 1) == 1
      sleep(baudPeriod)
    }
    parity match {
      case "odd"   => pin #= oddParity(b)
      case "even"  => pin #= evenParity(b)
      case "mark"  => pin #= true
      case "space" => pin #= false
    }
    sleep(baudPeriod)

    pin #= true
    sleep(stopBits * baudPeriod)
  }

  def send(x: BigInt, idleTime: () => Double = () => 0.0): Unit = {
    x.toBytes(endian = BIG).foreach { b =>
      send(b)
      sleep(idleTime())
    }
  }
}

class UartSimMonitor(
    pin: Bool,
    baudrate: HertzNumber,
    parity: String = "odd",
    stopBits: Double = 1.0
) {
  val baudPeriod =
    (baudrate.toTime / TimeNumber(SimManagerContext.current.manager.timePrecision)).toLong
  assert(baudPeriod > 0, "can't simulate uart datarate with given simulator timescale")

  def run(cb: (BigInt, Boolean) => Unit) = {
    fork {
      sleep(1)
      while (true) {
        val (data, parityValid) = rxByte()
        cb(data, parityValid)
      }
    }
  }

  def rxByte(): (BigInt, Boolean) = {
    waitUntil(!pin.toBoolean)
    sleep(baudPeriod / 2)

    var data = 0
    var evenParity = false
    for (idx <- 0 to 7) {
      sleep(baudPeriod)
      data |= (if (pin.toBoolean) 1 else 0) << idx
      evenParity ^= pin.toBoolean
    }

    sleep(baudPeriod)
    val rxdParity = pin.toBoolean
    sleep(0.5 * baudPeriod)

    sleep(stopBits * baudPeriod)

    parity match {
      case "odd"   => (data, rxdParity != evenParity)
      case "even"  => (data, rxdParity == evenParity)
      case "mark"  => (data, rxdParity)
      case "space" => (data, !rxdParity)
    }
  }

  def rx(bytes: Int): BigInt = {
    var result = BigInt(0)
    for (_ <- 0 until bytes) {
      val (data, parityValid) = rxByte()
      assert(parityValid, "invalid parity received")
      result = (result << 8) + data
    }
    result
  }
}

abstract class Apb3Monitor(apb: Apb3, clockDomain: ClockDomain, individualBytes: Boolean = false) {
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
        if (individualBytes) {
          val bytes = new Array[Byte](apb.config.dataWidth / 8 + 1)
          for (i <- 0 until bytes.length - 1) {
            bytes(i + 1) = onRead(address + apb.config.dataWidth / 8 - 1 - i).toByte
          }
          apb.PRDATA #= BigInt(bytes)
        } else {
          apb.PRDATA #= onRead(address)
        }
      } else {
        if (individualBytes) {
          val bytes = apb.PWDATA.toBigInt.toByteArray.reverse
          for (i <- 0 until apb.config.dataWidth / 8) {
            onWrite(address + i, if (bytes.length > i) bytes(i) else 0)
          }
        } else {
          onWrite(address, apb.PWDATA.toBigInt)
        }
      }
    }
    if (ready_delay > 0) {
      ready_delay -= 1
    }
  }

  def onProtocolError(text: String): Unit
  def nextDelay() = Random.nextInt(5)
  def onRead(address: BigInt): BigInt
  def onWrite(address: BigInt, value: BigInt): Unit
}

class UartApbBridgeTest extends SpinalFunSuite {
  val baudrate = 100 MHz

  case class Bench(private val dut: UartApbBridge) {
    val tx = new UartSimDriver(dut.io.uart.rxd, baudrate)

    val rxBytes = mutable.ArrayBuffer[BigInt]()
    new UartSimMonitor(dut.io.uart.txd, baudrate).run { (data, parityValid) =>
      assert(parityValid, "data received with invalid parity")
      rxBytes.append(data)
    }
    def rx(byteCnt: Int) = {
      waitUntil(rxBytes.length >= byteCnt)
      val result = rxBytes.take(byteCnt).reduceLeft[BigInt]((old, bytes) => (old << 8) + bytes)
      rxBytes.trimStart(byteCnt)
      result
    }

    val readScoreboard = LoggingScoreboardInOrder[(BigInt, BigInt)]("read")
    val writeScoreboard = LoggingScoreboardInOrder[(BigInt, BigInt)]("write")
    new Apb3Monitor(dut.io.apb, dut.clockDomain, individualBytes = false) {
      override def onProtocolError(text: String): Unit = fail(text)

      override def onRead(address: BigInt) = {
        val result = dut.io.apb.PWDATA.randomizedBigInt()
        readScoreboard.pushDut((address, result))
        result
      }

      override def onWrite(address: BigInt, value: BigInt): Unit = {
        writeScoreboard.pushDut((address, value))
      }
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(10)

    def finish(): Unit = {
      sleep(1000)
      readScoreboard.checkEmptyness()
      writeScoreboard.checkEmptyness()
    }
  }
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 GHz))
    )
    .compile(
      UartApbBridge(
        UartApbBridgeGenerics(
          Apb3Config(addressWidth = 8, dataWidth = 16),
          baudrate = baudrate.toInt
        )
      )
    )

  test(dut, "read one register") { dut =>
    val bench = Bench(dut)
    import bench._

    SimTimeout(100 us)

    10 times {
      val address = Random.nextInt(0xff)
      tx.send(0x2000 + address) // read
      assert(rx(1) == 0x20, "response header not received")
      val read = rx(2)
      readScoreboard.pushRef((address, read))
    }

    finish()
  }

  test(dut, "write one register") { dut =>
    val bench = Bench(dut)
    import bench._

    SimTimeout(100 us)

    10 times {
      val address = Random.nextInt(0xff)
      val data = Random.nextInt(0xffff) | 0x8001
      simLog(f"writing $address%04x = $data%04x")
      tx.send(0x4000 + address) // write
      tx.send(data)
      assert(rx(1) == 0x40, "response header not received")
      writeScoreboard.pushRef((address, data))
    }

    finish()
  }
}
