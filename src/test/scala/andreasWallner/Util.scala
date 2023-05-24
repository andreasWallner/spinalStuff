package andreasWallner

import andreasWallner.sim.simLog
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.ScoreboardInOrder

import scala.util.Random

case class SequenceSource(elements: Seq[Int]) {
  var idx = 0

  def apply(payload: Bits): Boolean = {
    if (idx < elements.size) {
      payload #= elements(idx)
      idx = idx + 1
      true
    } else {
      false
    }
  }

  val done = idx == elements.size
}

case class RandomSource(elementCnt: Long, maxRnd: Int) {
  var cnt = 0

  def apply(payload: Bits): Boolean = {
    if (cnt < elementCnt) {
      payload #= Random.nextInt(maxRnd)
      cnt += 1
      true
    } else {
      false
    }
  }

  val done = cnt == elementCnt
}

/*
 * Randomize a passed payload n times, use e.g. for StreamDrivers
 *
 *     StreamDriver(dut.io.tx, dut.clockDomain)(PayloadRandmizer(toSend).apply)
 */
case class PayloadRandmizer(elementCnt: Long) {
  var cnt = 0

  def apply(payload: Bits): Boolean = {
    if (cnt < elementCnt) {
      payload.randomize()
      cnt += 1
      true
    } else {
      false
    }
  }

  def done: Boolean = elementCnt == cnt
}

object LoggingScoreboardInOrder {
  def noLogFn[T](isRef: Boolean, data: T): Unit = {}
  def simLogFn[T](name: String)(isRef: Boolean, data: T): Unit = {
    val color = if (isRef) Console.BLUE else Console.GREEN
    val dirStr = if (isRef) "ref" else "  dut"
    simLog(s"$name$color$dirStr${Console.RESET}", data)
  }
  def fmtLogFn[T](name: String, fmtFn: T => String)(isRef: Boolean, data: T): Unit = {
    val color = if (isRef) Console.BLUE else Console.GREEN
    val dirStr = if (isRef) "ref" else "  dut"
    simLog(s"$name$color$dirStr${Console.RESET}", fmtFn(data))
  }
  def apply[T]() = new LoggingScoreboardInOrder[T](simLogFn[T](""))
  def apply[T](name: String) = new LoggingScoreboardInOrder[T](simLogFn[T](name + " "))
  def apply[T](logFn: (Boolean, T) => Unit) = new LoggingScoreboardInOrder[T](logFn)
  def apply[T](name: String, fmtFn: T => String) =
    new LoggingScoreboardInOrder[T](fmtLogFn(name + " ", fmtFn))
  def apply[T](enable: Boolean, name: String) =
    new LoggingScoreboardInOrder[T](if(!enable) noLogFn else simLogFn[T](name + " "))
  def apply[T](enable: Boolean, name: String, fmtFn: T => String) =
    new LoggingScoreboardInOrder[T](if(!enable) noLogFn else fmtLogFn(name + " ", fmtFn))
}

class LoggingScoreboardInOrder[T](logFn: (Boolean, T) => Unit) extends ScoreboardInOrder[T] {
  override def pushRef(that: T): Unit = {
    logFn(true, that)
    super.pushRef(that)
  }

  override def pushDut(that: T): Unit = {
    logFn(false, that)
    super.pushDut(that)
  }
}
