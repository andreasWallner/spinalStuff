package andreasWallner.io.i3c.sim

import andreasWallner.Utils.oddParity
import spinal.core._
import spinal.core.sim._
import andreasWallner.io.i3c.I3C
import andreasWallner.sim.SimTriStatePimper

abstract class Event
case class Start(repeated: Boolean) extends Event
case class Stop() extends Event
case class Bit(data: Boolean) extends Event
case class Byte(data: Int) extends Event
case class ReadAbort() extends Event

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
      if(!repeatedStart)
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

  def rxBits(bitCnt: Int): Event = {
    val bits = for (_ <- 0 until bitCnt) yield {
      waitUntil(i3c.scl.read.toBoolean)
      val stopPossible = !i3c.sda.read.toBoolean
      waitUntil(!i3c.scl.read.toBoolean || (stopPossible == i3c.sda.read.toBoolean))
      (stopPossible, i3c.sda.read.toBoolean) match {
        case (true, true) =>
          seenStop()
          return Stop()
        case (false, false) =>
          seenStart(true)
          return Start(true)
        case (_, b) =>
          event(Bit(b))
      }
      i3c.sda.read.toBoolean
    }
    Byte(toInt(bits, msbFirst = true))
  }

  // expected: SCL is high & SDA low
  def waitForStopOrRepeatedStart() = {
    assert(i3c.scl.read.toBoolean)
    assert(!i3c.sda.read.toBoolean)
    waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
    val isStop = !i3c.sda.read.toBoolean
    if(isStop) {
      event(Stop())
      Stop()
    } else {
      waitUntil(i3c.scl.read.toBoolean || !i3c.sda.read.toBoolean)
      if(!i3c.sda.read.toBoolean)
        throw new Exception("SDA changed while waiting for repeated start initial SCL change")
      waitUntil(!i3c.scl.read.toBoolean || !i3c.sda.read.toBoolean)
      if(!i3c.scl.read.toBoolean)
        throw new Exception("SCL changed while waiting for repeated start SDA change")
      waitUntil(!i3c.scl.read.toBoolean || i3c.sda.read.toBoolean)
      if(i3c.sda.read.toBoolean)
        throw new Exception("SDA changed back while waiting for repeated start ending SCL change")
      Start(true)
    }
  }

  def doRxTx(repeatedStart: Boolean): Boolean = {
    val startTime = simTime()

    val header = rxBits(8)
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
        (isRead, seenHeader(data >> 1, isRead, repeated = repeatedStart))
    }

    if (ack)
      i3c.sda.drive(false)
    waitUntil(i3c.scl.read.toBoolean)
    // see 5.1.2.3.1 p. 34
    i3c.sda.highz()
    waitUntil(!i3c.scl.read.toBoolean)

    if (ack && isRead) {
      assert(response.nonEmpty)
      for(idx <- response.indices) {
        val last = idx == response.size - 1
        txByte(response(idx), last)
        // txbyte left with SCL high and last driven -> handle repeated start/stop to abort
        if(!last) {
          i3c.sda.highz()
          waitUntil(i3c.scl.read.toBoolean)
          waitUntil(!i3c.sda.read.toBoolean || !i3c.scl.read.toBoolean)
          // TODO check tCBSr
          val hostAbort = !i3c.sda.read.toBoolean
          if(hostAbort) {
            event(ReadAbort())
            seenReadAbort()
            return true
          }
        } else {
          // TODO verify that host drives low/overlapping (5.1.2.3.3 "i3c master shall drive SDA Low")
          return waitForStopOrRepeatedStart() match {
            case Stop() => false
            case Start(true) => true
          }
        }


      }
    } else if(!isRead) {
      while (true) {
        val byte = rxBits(9)
        event(byte)
        byte match {
          case Stop() =>
            seenStop()
            return false
          case Start(true) =>
            seenStart(true)
            return true
          case Byte(data) =>
            seenSDA(data >> 1, oddParity(data >> 1) == (data & 1).toBoolean)
        }
      }
    }
    throw new Exception("invalid sequence - we should never be here")
  }

  /**
   * Unlike other functions this returns with scl high to allow for detection of repeated start/stop
   **/
  def txByte(data: Int, last: Boolean): Unit = {
    // TODO add 12 ns delay...
    for(i <- 7 downto 0) {
      val bit = ((data >> i) & 1).toBoolean
      // TODO allow for hard 1
      if(bit) i3c.sda.highz() else i3c.sda.drive(false)
      waitUntil(i3c.scl.read.toBoolean)
      waitUntil(!i3c.scl.read.toBoolean)
    }
    // see 5.1.2.3.3 p. 35
    i3c.sda.drive(last)
    waitUntil(i3c.scl.read.toBoolean)
    i3c.sda.highz()
  }

  def event(e: Event): Unit = {}
  def seenStart(repeated: Boolean): Unit = {}
  def seenHeader(address: Int, RnW: Boolean, repeated: Boolean): (Boolean, Seq[Int]) = (false, Seq())
  def sent(data: Int): Unit = {}
  def seenSDA(data: Int, validParity: Boolean): Unit = {}
  def seenStop(): Unit = {}
  def seenReadAbort(): Unit = {}
}
