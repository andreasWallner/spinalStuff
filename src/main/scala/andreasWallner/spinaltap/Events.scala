package andreasWallner.spinaltap

import spinal.core._
import spinal.lib._

case class Event() extends Bundle {
  val source = UInt(8 bit)
  val data = Bits(16 bit)
}

object EventStream {
  def apply() = Stream(Fragment(Bits(16 bit)))
}