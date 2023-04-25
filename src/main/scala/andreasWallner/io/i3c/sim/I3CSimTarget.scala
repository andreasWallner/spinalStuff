package andreasWallner.io.i3c.sim

import andreasWallner.Utils.hasOddParity
import andreasWallner.io.i3c.I3C
import andreasWallner.sim.{SimTriStatePimper, simLog}
import andreasWallner.util.IterablePimper
import spinal.core._
import spinal.core.sim._

abstract class Event
case class Start(repeated: Boolean) extends Event
case class Stop() extends Event
case class Bit(data: Boolean) extends Event
case class Byte(data: Int) extends Event
case class Passive(data: Int) extends Event
case class ReadAbort() extends Event
case class ParityError(b: Byte) extends Event

case class I3CSimTarget(i3c: I3C, cd: ClockDomain) {
  def toInt(bits: Seq[Boolean], msbFirst: Boolean): Int = {
    (if (msbFirst) bits else bits.reverse).foldLeft(0)((i: Int, b: Boolean) =>
      (i << 1) + (if (b) 1 else 0)
    )
  }

  sleep(1)
  fork {
    var repeatedStart = false
    while (true) {
      if (!repeatedStart)
        waitForStart()
      repeatedStart = doRxTx(repeatedStart)
    }
  }
  // todo irq
  // todo hotjoin

  def waitForStart(): Unit = {
    waitUntil(!i3c.sda.read.toBoolean)
    assert(i3c.scl.read.toBoolean, "SCL not high during suspected Start")
    waitUntil(!i3c.scl.read.toBoolean)
    event(Start(false))
    seenStart(false)
    // val holdTimeSTA = simTime() - startTime
    // TODO assert(holdTimeSTA > 260 ns) // p. 140 3068
    // TODO verify tLow
    // TODO verify open-drain
  }

  def rxBits(bitCnt: Int, passive: Boolean = false, requireOpenDrain: Boolean = false): Event = {
    val bits = for (i <- 0 until bitCnt) yield {
      waitUntil(i3c.scl.read.toBoolean)
      val bitLevel = i3c.sda.read.toBoolean
      val correctMode = if (requireOpenDrain) i3c.sda.isOpenDrain else i3c.sda.isPushPull
      simLog(correctMode, i3c.sda.isOpenDrain, i3c.sda.isPushPull)

      val changedSda = !i3c.sda.read.toBoolean
      waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean == changedSda)
      (i3c.scl.read.toBoolean, i3c.sda.read.toBoolean) match {
        case (false, _) =>
          if (!correctMode)
            throw new Exception(
              "Drive mode not " + (if (requireOpenDrain) "open-drain" else "push-pull") + " as required"
            )

          event(Bit(bitLevel))
        case _ if i != 0 =>
          throw new Exception("P/Sr only allowed at first bit")
        case (true, true) =>
          seenStop()
          return Stop()
        case (true, false) =>
          waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean != changedSda)
          if (i3c.scl.read.toBoolean)
            throw new Exception("invalid SDA transition found during Sr")
          seenStart(true)
          return Start(true)
      }
      bitLevel
    }
    if (!passive)
      Byte(toInt(bits, msbFirst = true))
    else
      Passive(toInt(bits, msbFirst = true))
  }

  // decide between Sr and P
  def waitForStopOrRepeatedStart() = {
    assert(
      !i3c.scl.read.toBoolean,
      "expected scl to be low when waiting for stop or repeated start"
    )
    assert(
      !i3c.sda.read.toBoolean,
      "expected sda to be low when waiting for stop or repeated start"
    )
    waitUntil(i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
    val isStop = i3c.scl.read.toBoolean
    if (isStop) {
      waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
      if (!i3c.scl.read.toBoolean)
        throw new Exception("SCL changed when SDA change was expected for stop")
      event(Stop())
      Stop()
    } else {
      waitUntil(i3c.scl.read.toBoolean || !i3c.sda.read.toBoolean)
      if (!i3c.sda.read.toBoolean)
        throw new Exception("SDA changed while waiting for repeated start initial SCL change")
      waitUntil(!i3c.scl.read.toBoolean || !i3c.sda.read.toBoolean)
      if (!i3c.scl.read.toBoolean)
        throw new Exception("SCL changed while waiting for repeated start SDA change")
      waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
      if (i3c.sda.read.toBoolean)
        throw new Exception("SDA changed back while waiting for repeated start ending SCL change")
      event(Start(true))
      Start(true)
    }
  }

  def doRxTx(repeatedStart: Boolean): Boolean = {
    val startTime = simTime()

    val header = rxBits(8, requireOpenDrain = !repeatedStart)
    event(header)
    val (isRead, (ack, response)) = header match {
      case Stop() =>
        seenStop()
        return false
      case Start(true) =>
        seenStart(true)
        return true
      case Byte(data) =>
        val isRead = (data & 1).toBoolean
        (isRead, addressReaction(data >> 1, isRead, repeated = repeatedStart))
    }
    // TODO validate isRead/ack/response combination

    if (ack)
      i3c.sda.drive(false)
    waitUntil(i3c.scl.read.toBoolean)
    // see 5.1.2.3.1 p. 34
    // if write, let the controller take over signalling, in NACK case we are already highz
    if (!isRead)
      i3c.sda.highz()
    waitUntil(!i3c.scl.read.toBoolean)

    if (!ack) {
      while (true) {
        val byte = rxBits(9, passive = true)
        event(byte)
        byte match {
          case Stop() =>
            seenStop()
            return false
          case Start(true) =>
            seenStart(true)
            return true
          case Passive(_) =>
          case _          => throw new Exception(f"unexpected event $byte while passively receiving")
        }
      }
    } else if (isRead) {
      assert(response.nonEmpty, "instructed sim target to respond, but provided no data")
      for ((byte, last) <- response.zipWithIsLast) {
        val controllerEnded = txByte(byte, last)
        if (last) {
          return waitForStopOrRepeatedStart() match {
            case Stop()      => false
            case Start(true) => true
          }
        } else if (controllerEnded) {
          return true
        }
      }
    } else {
      while (true) {
        val byte = rxBits(9)
        val parityChecked = byte match {
          case Byte(data) if hasOddParity(data) => Byte(data >> 1)
          case b: Byte                          => ParityError(b)
          case _                                => byte
        }
        parityChecked match {
          case Stop() => delayed(9999) { event(parityChecked) }
          case _      => event(parityChecked)
        }

        byte match {
          case Stop() =>
            seenStop()
            return false
          case Start(true) =>
            seenStart(true)
            return true
          case Byte(data) =>
            seenSDA(data)
        }
      }
    }
    throw new Exception("invalid sequence - we should never be here")
  }

  def txByte(data: Int, last: Boolean) = {
    // TODO add 12 ns delay...
    for (i <- 7 downto 0) {
      val bit = ((data >> i) & 1).toBoolean
      i3c.sda.drive(bit)
      waitUntil(i3c.scl.read.toBoolean)
      waitUntil(!i3c.scl.read.toBoolean)
    }
    // see 5.1.2.3.4 p. 56 & Figure
    if (last) {
      i3c.sda.drive(false)
      waitUntil(i3c.scl.read.toBoolean)
      i3c.sda.highz()
      waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
      if (i3c.sda.read.toBoolean)
        throw new Exception("Controller did not extend end of message at end of transmitted byte")
      false
    } else {
      i3c.sda.drive(true)
      waitUntil(i3c.scl.read.toBoolean)
      i3c.sda.highz()
      waitUntil(!i3c.scl.read.toBoolean || !i3c.scl.read.toBoolean)
      if (!i3c.sda.read.toBoolean) {
        event(Start(true))
        waitUntil(!i3c.scl.read.toBoolean)
        true
      } else {
        false
      }
    }
  }

  def event(e: Event): Unit = {}
  def seenStart(repeated: Boolean): Unit = {}
  def addressReaction(address: Int, RnW: Boolean, repeated: Boolean): (Boolean, Seq[Int]) =
    (false, Seq())
  def sent(data: Int): Unit = {}
  def seenSDA(data: Int): Unit = {}
  def seenStop(): Unit = {}
  def seenReadAbort(): Unit = {}
}
