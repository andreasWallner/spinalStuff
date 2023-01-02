package andreasWallner.io.iomux

import spinal.core._
import spinal.core.sim._
import andreasWallner.SpinalFunSuite
import andreasWallner.io.iomux.IOMux.PortGenerics
import andreasWallner.sim.PimpedSpinalSimConfig
import spinal.core.sim.SpinalSimConfig
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}

import scala.collection.mutable
import scala.language.postfixOps

case class IOMuxCoreTester() extends Component {
  val g = IOMux.Generics(
    inPorts = 3,
    outPorts = 2,
    portGenerics = PortGenerics(triCnt = 2, outCnt = 1),
    withSwap = true
  )
  val io = new Bundle {
    val in_0_write = in(Vec(Bool(), 2))
    val in_0_writeEnable = in(Vec(Bool(), 2))
    val in_0_read = out(Vec(Bool(), 2))

    val in_1_write = in(Vec(Bool(), 2))
    val in_1_writeEnable = in(Vec(Bool(), 2))
    val in_1_read = out(Vec(Bool(), 2))

    val in_2_write = in(Vec(Bool(), 2))
    val in_2_writeEnable = in(Vec(Bool(), 2))
    val in_2_read = out(Vec(Bool(), 2))

    val out_0_write = out(Vec(Bool(), 2))
    val out_0_writeEnable = out(Vec(Bool(), 2))
    val out_0_read = in(Vec(Bool(), 2))

    val out_1_write = out(Vec(Bool(), 2))
    val out_1_writeEnable = out(Vec(Bool(), 2))
    val out_1_read = in(Vec(Bool(), 2))

    val sels = in Vec(UInt(g.selWidth bits), g.outPorts)
    val swapSel =
      if (g.withSwap)
        in(Vec(Vec(UInt(g.swapWidth bits), g.portGenerics.triCnt), g.inPorts))
      else null
  }

  val d = IOMux.Core(
    g
  )

  io.in_0_read.assignFromBits(d.io.all(0).tri.read)
  d.io.all(0).tri.write := io.in_0_write.asBits
  d.io.all(0).tri.writeEnable := io.in_0_writeEnable.asBits
  d.io.all(0).o.clearAll()

  io.in_1_read.assignFromBits(d.io.all(1).tri.read)
  d.io.all(1).tri.write := io.in_1_write.asBits
  d.io.all(1).tri.writeEnable := io.in_1_writeEnable.asBits
  d.io.all(1).o.clearAll()

  io.in_2_read.assignFromBits(d.io.all(2).tri.read)
  d.io.all(2).tri.write := io.in_2_write.asBits
  d.io.all(2).tri.writeEnable := io.in_2_writeEnable.asBits
  d.io.all(2).o.clearAll()

  io.out_0_write.assignFromBits(d.io.muxeds(0).tri.write)
  io.out_0_writeEnable.assignFromBits(d.io.muxeds(0).tri.writeEnable)
  d.io.muxeds(0).tri.read := io.out_0_read.asBits

  io.out_1_write.assignFromBits(d.io.muxeds(1).tri.write)
  io.out_1_writeEnable.assignFromBits(d.io.muxeds(1).tri.writeEnable)
  d.io.muxeds(1).tri.read := io.out_1_read.asBits

  d.io.sels := io.sels
  d.io.swapSel.get := io.swapSel
}

class IOMuxCoreTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(IOMuxCoreTester())

  test(dut, "test connections") { dut =>
    def check(i: Bool, o: Bool): Unit = {
      for (ii <- 0 to 5) {
        i.randomize()
        dut.clockDomain.waitSampling(3)
        assert(o.toBoolean == i.toBoolean)
      }

    }

    for (i <- 0 until dut.g.inPorts; t <- 0 until dut.g.portGenerics.triCnt) {
      dut.io.swapSel(i)(t) #= t
    }
    for (i <- 0 until dut.g.outPorts) {
      dut.io.sels(i) #= i
    }
    dut.clockDomain.forkStimulus(10)

    check(dut.io.in_0_write(0), dut.io.out_0_write(0))
    check(dut.io.in_0_write(1), dut.io.out_0_write(1))
    check(dut.io.in_1_write(0), dut.io.out_1_write(0))
    check(dut.io.out_0_read(0), dut.io.in_0_read(0))
    check(dut.io.out_1_read(1), dut.io.in_1_read(1))
    check(dut.io.in_0_writeEnable(0), dut.io.out_0_writeEnable(0))
    check(dut.io.in_0_writeEnable(1), dut.io.out_0_writeEnable(1))
    check(dut.io.in_1_writeEnable(0), dut.io.out_1_writeEnable(0))

    dut.io.sels(0) #= 1
    dut.io.sels(1) #= 0
    check(dut.io.in_0_write(0), dut.io.out_1_write(0))
    check(dut.io.in_1_write(1), dut.io.out_0_write(1))
    check(dut.io.out_0_read(0), dut.io.in_1_read(0))
    check(dut.io.out_1_read(1), dut.io.in_0_read(1))
    check(dut.io.in_0_writeEnable(0), dut.io.out_1_writeEnable(0))
    check(dut.io.in_1_writeEnable(1), dut.io.out_0_writeEnable(1))

    dut.io.swapSel(0)(0) #= 1
    dut.io.swapSel(0)(1) #= 0
    check(dut.io.in_0_write(1), dut.io.out_1_write(0))
    check(dut.io.in_0_write(0), dut.io.out_1_write(1))
    check(dut.io.in_1_write(0), dut.io.out_0_write(0))
    check(dut.io.out_1_read(0), dut.io.in_0_read(1))
    check(dut.io.out_1_read(1), dut.io.in_0_read(0))
    check(dut.io.out_0_read(0), dut.io.in_1_read(0))
    check(dut.io.in_0_writeEnable(1), dut.io.out_1_writeEnable(0))
    check(dut.io.in_0_writeEnable(0), dut.io.out_1_writeEnable(1))
    check(dut.io.in_1_writeEnable(0), dut.io.out_0_writeEnable(0))

    dut.io.sels(0) #= 0
    dut.io.swapSel(0)(0) #= dut.io.swapSel(0)(0).maxValue
    dut.io.in_0_writeEnable(0) #= true
    dut.clockDomain.waitSampling()
    assert(!dut.io.out_0_writeEnable(0).toBoolean)
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
    .compile(new IOMux.Ctrl[Apb3](generics, Apb3(8, 32), new Apb3SlaveFactory(_, 0)))

  test(dut, "smoketest") { dut =>
    val driver = Apb3Driver(dut.io.bus, dut.clockDomain)
    dut.clockDomain.forkStimulus(10)

    driver.write(0x04, 0xffffffffL)

    // TODO WIP do some useful checks here

    dut.clockDomain.waitSampling(100)
  }
}
