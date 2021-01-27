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

  List((true, true), (true, false), (false, true), (false, false)).foreach {
    case (cpol: Boolean, cpha: Boolean) =>
      val name = f"loopback cpol=$cpol cpha=$cpha"
      test(name) {
        dut.doSim(name, seed = 1) { dut =>
          dut.io.spi.miso #= false
          dut.io.config.prescaler #= 100
          dut.io.config.spiType.cpol #= cpol
          dut.io.config.spiType.cpha #= cpha
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
            payload.fragment.randomize()
            payload.last #= (scoreboard.matches + scoreboard.ref.length >= toSend - 2)
            !(scoreboard.matches + scoreboard.ref.length > toSend - 2)
          }
          StreamMonitor(dut.io.txData, dut.clockDomain) { payload =>
            println(f"@${simTime()} ref ${payload.fragment.toInt} 0x${payload.fragment.toInt}%x")
            scoreboard.pushRef(payload.fragment.toInt)
          }
          driver.delay = 0
          driver.transactionDelay = () => {
            0
          }
          FlowMonitor(dut.io.rxData, dut.clockDomain) { payload =>
            println(f"@${simTime()} dut ${payload.toInt} 0x${payload.toInt}%x")
            scoreboard.pushDut(payload.toInt)
          }

          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.waitActiveEdge(100)
          dut.io.start #= true
          dut.clockDomain.waitActiveEdge()
          dut.io.start #= false
          dut.clockDomain.waitActiveEdge(10000)
          scoreboard.checkEmptyness()
          assert(scoreboard.matches == toSend)
        }
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
    dut.doSim("simple") { dut =>
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
