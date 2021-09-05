package andreasWallner.spinaltap

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba3.apb.sim.Apb3Driver

class SpinalTapTest extends AnyFunSuite {
  val dut = SimConfig.withWave.compile(new ApbSpinalTap())

  test("configure and run ISO communication") {
    dut.doSim("configure and run ISO communication") { dut =>
      SimTimeout(100000)
      val driver = Apb3Driver(dut.io.bus, dut.clockDomain)
      println(dut.io.port0);
      //dut.io.port0(0).read #= true // IO
      dut.clockDomain.forkStimulus(10)

      val isobase = 0x43c00400;
      val muxbase = 0x43c00300;

      driver.write(muxbase, 0x201)

      driver.write(isobase + 0x14, 50)
      driver.write(isobase + 0x44, 2000)
      driver.write(isobase + 0x48, 3000)

      driver.write(isobase + 0x18, 2000) // ta
      driver.write(isobase + 0x1c, 3000) // tb
      driver.write(isobase + 0x20, 1000) // te
      driver.write(isobase + 0x24, 1000) // th
      driver.write(isobase + 0x28, 1000) // vcc_offset
      driver.write(isobase + 0x2c, 1000) // clk_offset

      driver.write(isobase + 0x10, 0x11)

      while (driver.read(isobase + 0x08) != 0) {}
      dut.clockDomain.waitActiveEdge(10)
    }
  }
}
