package andreasWallner

import andreasWallner.sim.simLog
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.ScoreboardInOrder

import scala.util.Random

case class SequenceSource(elements: Seq[Int]) {
  var idx = 0

  def apply(payload: Bits): Boolean = {
    if(idx < elements.size) {
      payload #= elements(idx)
      idx = idx + 1
      true
    } else {
      false
    }
  }
}

case class RandomSource(elementCnt: Long, maxRnd: Int) {
  var cnt = 0

  def apply(payload: Bits): Boolean = {
    if(cnt < elementCnt) {
      payload #= Random.nextInt(maxRnd)
      cnt += 1
      true
    } else {
      false
    }
  }
}

/*
 * Randomize a passed payload n times, use e.g. for StreamDrivers
 *
 *     StreamDriver(dut.io.tx, dut.clockDomain)(PayloadRandmizer(toSend).apply)
 */
case class PayloadRandmizer(elementCnt: Long) {
  var cnt = 0

  def apply(payload: Bits): Boolean = {
    if(cnt < elementCnt) {
      payload.randomize()
      cnt += 1
      true
    } else {
      false
    }
  }
}

object LoggingScoreboardInOrder {
  def apply[T]() = new LoggingScoreboardInOrder[T]("")
  def apply[T](name:String) = new LoggingScoreboardInOrder[T](name)
}

class LoggingScoreboardInOrder[T](name:String) extends ScoreboardInOrder[T] {
  val spacedName = if(name != "") name + " " else ""
  override def pushRef(that: T): Unit = {
    simLog(spacedName + s"${Console.BLUE}ref${Console.RESET}", that)
    super.pushRef(that)
  }

  override def pushDut(that: T): Unit = {
    simLog(spacedName + s"${Console.GREEN}dut${Console.RESET}", that)
    super.pushDut(that)
  }
}
