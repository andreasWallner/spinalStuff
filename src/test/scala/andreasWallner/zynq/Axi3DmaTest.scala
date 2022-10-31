package andreasWallner.zynq

import andreasWallner.{PayloadRandmizer, SpinalFunSuite}
import andreasWallner.sim._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.amba4.axi.sim.{
  Axi4WriteOnlyMonitor,
  Axi4WriteOnlySlaveAgent
}
import spinal.lib.sim.{StreamDriver, StreamMonitor}

import scala.collection.mutable

object helper {
  def printMemory(m: mutable.HashMap[BigInt, Byte], width: Int = 16): Unit = {
    def first = m.keys.reduce((a, b) => if (a < b) a else b)

    def last = m.keys.reduce((a, b) => if (a > b) a else b)

    for (lineStart <- first to last by width) {
      print(f"0x$lineStart%04x")
      for (addr <- lineStart to last.min(lineStart + width)) {
        val v = m.get(addr).map(x => f" $x%02x").getOrElse("   ")
        print(v)
      }
      println()
    }
  }
}

class Axi3DmaTest extends SpinalFunSuite {
  val dut =
    SimConfig.withWaveOverride("fst").compile {
      Axi3Dma(Apb3Config(16, 32))
    }

  def setup(
             dut: Axi3Dma,
             dataSource: PayloadRandmizer,
             end: BigInt,
             circular: Boolean = false
           ) = {
    dut.io.config.startAddress #= 0x1000
    dut.io.config.endAddress #= end
    dut.io.config.circular #= circular
    dut.io.run #= false

    val memory = mutable.HashMap[BigInt, Byte]()
    val expected = mutable.HashMap[BigInt, Byte]()

    new Axi4WriteOnlySlaveAgent(dut.io.axi, dut.clockDomain)
    val monitor = new Axi4WriteOnlyMonitor(dut.io.axi, dut.clockDomain) {
      // @formatter:off
      override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
        //simLog()
        //simLog("write start address", f"$address%x", "id", id, "size", size, "len", len, "burst", burst)
      }

      override def onWriteByteAlways(address: BigInt, data: Byte, strobe: Boolean, id: Int): Unit = {
        //simLog("write always", f"$address%x", "data", f"${data & 0xff}%x  $data", "strobe", strobe, "id", id)
      }

      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        simLog("write", f"$address%x", f"${data & 0xff}%x")
        memory(address) = data
      }
      // @formatter:on
    }

    var expectedAddress = BigInt(0x1000)
    StreamDriver(dut.io.data, dut.clockDomain) {
      dataSource.apply
    }
    StreamMonitor(dut.io.data, dut.clockDomain) { payload =>
      //simLog(f"payload ${payload.toBigInt}%x")
      //simLog("width", payload.getBitsWidth)
      val bytes =
        (Array[Byte](0x00, 0x00) ++ payload.toBigInt.toByteArray).takeRight(2)
      simLog("bytes", bytes map (b => f"$b%x") mkString " ")
      expected(expectedAddress) = bytes(1)
      expected(expectedAddress + 1) = bytes(0)
      expectedAddress += 2
      if (dut.io.config.circular.toBoolean && expectedAddress >= dut.io.config.endAddress.toBigInt)
        expectedAddress = dut.io.config.startAddress.toBigInt
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(10)
    dut.io.run #= true

    (memory, expected, monitor, dataSource)
  }

  test(dut, "few writes") { dut =>
    SimTimeout(100000)
    val (memory, expected, monitor, randomizer) =
      setup(dut, PayloadRandmizer(0x10), 0x1100, circular = false)

    dut.clockDomain.waitActiveEdgeWhere(randomizer.done)
    dut.clockDomain.waitActiveEdgeWhere(!dut.io.busy.toBoolean)

    helper.printMemory(memory)
    helper.printMemory(expected)
    assert(!dut.io.hasWrapped.toBoolean)
    assert(!dut.io.full.toBoolean)
    assert(memory.size == 0x20)
    assert(memory == expected)
    assert(monitor.wProcess.isEmpty)
    assert(monitor.wQueue.isEmpty)
  }

  test(dut, "linear write must end") { dut =>
    SimTimeout(100000)
    val (memory, expected, monitor, randomizer) =
      setup(dut, PayloadRandmizer(0x100), 0x1020, circular = false)

    dut.clockDomain.waitActiveEdgeWhere(dut.io.full.toBoolean)

    helper.printMemory(memory)
    helper.printMemory(expected)
    assert(dut.io.full.toBoolean)
    assert(memory.size == 0x20)
    assert(memory == expected)
    assert(monitor.wProcess.isEmpty)
    assert(monitor.wQueue.isEmpty)
  }

  test(dut, "wraparound") { dut =>
    SimTimeout(100000)
    val (memory, expected, monitor, randomizer) =
      setup(dut, PayloadRandmizer(0x100), 0x1020, circular = true)

    dut.clockDomain.waitActiveEdgeWhere(randomizer.done)
    dut.clockDomain.waitActiveEdgeWhere(!dut.io.busy.toBoolean)

    helper.printMemory(memory)
    helper.printMemory(expected)
    assert(dut.io.hasWrapped.toBoolean)
    assert(!dut.io.full.toBoolean)
    assert(memory.size == 0x20)
    assert(memory == expected)
    assert(monitor.wProcess.isEmpty)
    assert(monitor.wQueue.isEmpty)
  }
}
