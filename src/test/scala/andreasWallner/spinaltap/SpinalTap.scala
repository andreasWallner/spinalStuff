package andreasWallner.spinaltap

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba3.apb.sim.Apb3Driver

class SpinalTapTest extends AnyFunSuite {
  val dut = SimConfig.withWave.compile(new ApbSpinalTap())

  test("try write register") {
    dut.doSim("try write register") { dut =>
      SimTimeout(1000)
      val driver = Apb3Driver(dut.io.bus, dut.clockDomain)
      dut.clockDomain.forkStimulus(10)

      driver.write(0x43c00300, 0x201)

      driver.write(0x43c00104, 0)
      driver.write(0x43c00108, 0xff)
      driver.write(0x43c00204, 0)
      driver.write(0x43c00208, 0xff)

      driver.write(0x43c00104, 1)
      driver.write(0x43c00104, 0)
      driver.write(0x43c00104, 1)
      driver.write(0x43c00104, 0)

      driver.write(0x43c00204, 1)
      driver.write(0x43c00204, 0)
      driver.write(0x43c00204, 1)
      driver.write(0x43c00204, 0)

      dut.clockDomain.waitActiveEdge(10)

    }
  }
}
