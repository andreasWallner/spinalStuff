package andreasWallner.io.iomux

import spinal.core._
import spinal.core.sim._
import andreasWallner.SpinalFunSuite
import andreasWallner.io.iomux.IOMux.{MuxedPort, PortGenerics}
import andreasWallner.sim.{PimpedSpinalSimConfig, simLog}
import org.scalatest.funsuite.AnyFunSuiteLike
import spinal.core.sim.SpinalSimConfig
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.amba3.apb.sim.Apb3Driver

import scala.language.postfixOps

case class Helper(i: Vec[MuxedPort], o: Vec[MuxedPort], cd: ClockDomain, suite: AnyFunSuiteLike) {
  import suite.assert
  implicit class BitExtractPimper(b: Bits) {
    def bit(idx: Int): Boolean = ((b.toBigInt >> idx) & 1) == 1
  }

  def check(inPort: Int, inPin: Int, outPort: Int, outPin: Int): Unit = {
    for (_ <- 0 to 10) {
      i(inPort).tri.write.randomize()
      i(inPort).tri.writeEnable.randomize()
      o(outPort).tri.read.randomize()

      // wait for output signals to be available
      cd.waitSampling(2)
      assert(i(inPort).tri.write.bit(inPin) == o(outPort).tri.write.bit(outPin))
      assert(i(inPort).tri.writeEnable.bit(inPin) == o(outPort).tri.writeEnable.bit(outPin))

      // wait for remaining input sync
      cd.waitSampling(1)
      assert(o(outPort).tri.read.bit(outPin) == i(inPort).tri.read.bit(inPin))
    }
  }
}

class IOMuxCoreTest extends SpinalFunSuite {
  val g = IOMux.Generics(
    inPorts = 3,
    outPorts = 2,
    portGenerics = PortGenerics(triCnt = 2, outCnt = 1),
    withSwap = true
  )
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(IOMux.Core(g))

  test(dut, "test connections", seed=1047363851) { dut =>
    val h = Helper(dut.io.all, dut.io.muxeds, dut.clockDomain, this)
    import h._

    for (i <- 0 until dut.g.inPorts; t <- 0 until dut.g.portGenerics.triCnt) {
      dut.io.swapSel.get(i)(t) #= t
    }
    for (i <- 0 until dut.g.outPorts) {
      dut.io.sels(i) #= i
    }
    dut.clockDomain.forkStimulus(10)

    simLog("0 -> 0 and 1 -> 1, all pins straight through")
    check(0, 0, 0, 0)
    check(0, 1, 0, 1)
    check(1, 0, 1, 0)

    simLog("0 -> 1 and 1 -> 0, all pins straight through")
    dut.io.sels(0) #= 1
    dut.io.sels(1) #= 0
    check(0, 0, 1, 0)
    check(1, 1, 0, 1)

    simLog("0 -> 1 and 1 -> 0, pin 0 & 1 of port 0 swapped")
    dut.io.swapSel.get(0)(0) #= 1
    dut.io.swapSel.get(0)(1) #= 0
    check(0, 1, 1, 0)
    check(0, 0, 1, 1)
    check(1, 0, 0, 0)

    // verify that max swapSel disables output driver
    dut.io.sels(0) #= 0
    dut.io.swapSel.get(0)(0) #= dut.io.swapSel.get(0)(0).maxValue
    dut.io.all(0).tri.writeEnable #= 1
    dut.clockDomain.waitSampling(2) // one clock for input to propagate, one for register
    assert(!dut.io.muxeds(0).tri.writeEnable.bit(0))
  }
}

class IOMuxCtrlTest extends SpinalFunSuite {
  val generics = IOMux.Generics(
    inPorts = 3,
    outPorts = 2,
    portGenerics = PortGenerics(triCnt = 2, outCnt = 1),
    withSwap = true
  )
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(IOMux.Ctrl[Apb3](generics, Apb3(8, 32), new Apb3SlaveFactory(_, 0)))

  test(dut, "smoketest") { dut =>
    val h = Helper(dut.io.all, dut.io.muxeds, dut.clockDomain, this)
    import h._

    val driver = Apb3Driver(dut.io.bus, dut.clockDomain)
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling()

    simLog("0 -> 0 and 1 -> 1, all pins straight through")
    driver.write(0x00, (1 << 8) + (0 << 0))
    check(0, 0, 0, 0)
    check(0, 1, 0, 1)
    check(1, 0, 1, 0)
    check(1, 1, 1, 1)

    simLog("0 -> 1 and 1 -> 0, all pins straight through")
    driver.write(0x00, (0 << 8) + (1 << 0))
    check(0, 0, 1, 0)
    check(0, 1, 1, 1)
    check(1, 0, 0, 0)
    check(1, 1, 0, 1)

    simLog("0 -> 1 and 1 -> 0, pin 0 & 1 of port 0 swapped")
    driver.write(0x04, (1 << 0) + (0 << 2))
    check(0, 0, 1, 1)
    check(0, 1, 1, 0)
    check(1, 0, 0, 0)
    check(1, 1, 0, 1)

    dut.clockDomain.waitSampling(100)
  }
}
