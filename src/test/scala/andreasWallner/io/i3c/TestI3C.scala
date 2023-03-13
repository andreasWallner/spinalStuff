package andreasWallner.io.i3c

import andreasWallner.SpinalFunSuite
import spinal.core.sim._

class TestI3C extends SpinalFunSuite {
  val dut = SimConfig.withFstWave.compile(I3CMaster())

  test(dut, "something") { dut =>

  }
}
