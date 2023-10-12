package andreasWallner.io

import spinal.core._

case class RgbLed() extends Bundle {
  val r, g, b = Bool()
}
