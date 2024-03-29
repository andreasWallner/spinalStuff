package andreasWallner.la.sim

import spinal.core._
import spinal.core.sim._

import scala.util.Random
import andreasWallner.sim._

case class DataDriver(
                       d: BitVector,
                       cd: ClockDomain,
                       maxTime: Int = 10,
                       b2bRate: Double = 0.2,
                       seqStart: Seq[BigInt] = List()
                     )(cb: (Int, BigInt) => Boolean) {
  fork {
    var end = false
    var cnt = 0
    while (!end) {
      cd.waitSampling()
      val waitTime = if (Random.nextDouble() > b2bRate) {
        // at least on cycle, otherwise we should have taken the other branch
        val waitTime = Random.nextInt(maxTime) + 1
        cd.waitSampling(waitTime)
        waitTime
      } else 0
      val newValue = if (cnt < seqStart.length) {
        d #= seqStart(cnt)
        seqStart(cnt)
      } else d.changed()
      cnt += 1
      end = !cb(waitTime, newValue)
    }
    simLog("ending DataDriver")
  }
}
