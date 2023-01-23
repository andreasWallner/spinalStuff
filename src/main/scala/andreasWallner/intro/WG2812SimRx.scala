package andreasWallner.intro

import andreasWallner.sim.simLog
import spinal.core._
import spinal.core.sim._

case class WG2812SimRx(d: Bool, highTime: Int, lowTime: Int)(cb: Int => Unit) {
  val threshold = (highTime + lowTime) / 2
  fork {
    sleep(1)
    while(true) {
      waitUntil(d.toBoolean)
      val riseTime = simTime()
      waitUntil(!d.toBoolean)
      val fallTime = simTime()
      bit(fallTime - riseTime > threshold)
    }
  }

  var bits = List[Boolean]()
  def bit(b: Boolean): Unit = {
    //println(f"${simTime()} rx bit $b")
    bits = bits :+ b
    if(bits.length == 8) {
      byte(bits)
      bits = List()
    }
  }

  def byte(bs: List[Boolean]): Unit = {
    val i = bs.foldLeft(0)((ii, b) => (ii << 1) + b.toInt)
    //println(f"${simTime()} rx byte $i%02x")
    cb(i)
  }
}
