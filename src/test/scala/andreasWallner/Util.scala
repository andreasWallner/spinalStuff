package andreasWallner

import spinal.core._
import spinal.core.sim._

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
