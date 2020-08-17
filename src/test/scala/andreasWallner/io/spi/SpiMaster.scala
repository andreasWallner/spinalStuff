package andreasWallner.io.spi

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable.Queue
import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class SpiMasterSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .compile(SpiMaster())

  test("loopback") {
    dut.doSim("loopback") { dut =>
      dut.io.spi.miso #= false
      dut.io.config.prescaler #= 100
      dut.io.config.spiType.cpol #= true
      dut.io.config.spiType.cpha #= true
      dut.io.txData.valid #= true
      dut.io.txData.payload.last #= true
      dut.io.txData.payload.fragment #= 0x55
      dut.io.start #= false

      dut.clockDomain.onActiveEdges {
        dut.io.spi.miso #= dut.io.spi.mosi.toBoolean
      }

      val toSend = 5
      val scoreboard = ScoreboardInOrder[Int]()
      val driver = StreamDriver(dut.io.txData, dut.clockDomain) { payload =>
        val last = (scoreboard.matches + scoreboard.ref.length >= toSend - 1)
        val valid = (scoreboard.matches + scoreboard.ref.length > toSend - 1)
        val nextVal = Random.nextInt() & ((1 << 8) - 1)
        payload.fragment #= nextVal
        payload.last #= last
        scoreboard.pushRef(nextVal)
        !valid
      }
      driver.delay = 0
      driver.transactionDelay = () => { 0 }
      FlowMonitor(dut.io.rxData, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload.toInt)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(100)
      dut.io.start #= true
      dut.clockDomain.waitActiveEdge()
      dut.io.start #= false
      dut.clockDomain.waitActiveEdge(10000)
      scoreboard.checkEmptyness()
    }
  }
}

class Apb3SpiMasterSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    )
    .compile(Apb3SpiMaster())

  test("simple") {
    dut.doSim("simple", seed=181827950) { dut =>
      val toSend = 10
      val prescaler = 20
      SimTimeout(10 * 8 * prescaler * toSend * 4)
      val apb = Apb3Driver(dut.io.bus, dut.clockDomain)
      val scoreboard = ScoreboardInOrder[Int]()

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(100)

      dut.clockDomain.onActiveEdges {
        dut.io.spi.miso #= dut.io.spi.mosi.toBoolean
      }

      apb.write(0x18, prescaler << 2)

      apb.write(0x24, 0xde)
      apb.write(0x24, 0xad)
      apb.write(0x1c, BigInt(1) << 31)

      for (_ <- 1 to toSend) {
        val r = Random.nextInt(256)
        scoreboard.pushRef(r)
        apb.write(0x24, r)
      }
      assert(apb.read(0x14) >> 16 == 10)
      apb.write(0x1c, 1)

      while ((apb.read(0x10) & 1) != 0) {}

      val toRead = apb.read(0x14) & 0xffff
      for (_ <- 1 to toRead.toInt) {
        val v = apb.read(0x20)
        assert((v & (BigInt(1) << 31)) != 0)
        scoreboard.pushDut((v & 0xff).toInt)
      }

      val v = apb.read(0x20)
      assert((v & (BigInt(1) << 31)) == 0)

      scoreboard.checkEmptyness()
    }
  }
}
