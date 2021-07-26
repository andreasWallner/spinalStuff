package andreasWallner.io.spi

import andreasWallner.io.spi.sim.SpiMonitor
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.sim._

import scala.language.postfixOps
import scala.util.Random

class SpiMasterSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .compile(SpiMaster.Core())

  List((true, true), (true, false), (false, true), (false, false)).foreach {
    case (cpol: Boolean, cpha: Boolean) =>
      val name = f"loopback cpol=$cpol cpha=$cpha"
      test(name) {
        dut.doSim(name) { dut =>
          val toSend = 5
          val divider = 100

          dut.io.spi.miso #= false
          dut.io.config.divider #= divider
          dut.io.config.spiType.cpol #= cpol
          dut.io.config.spiType.cpha #= cpha
          dut.io.config.wordGuardClocks #= 5
          dut.io.config.msbFirst #= false
          dut.io.config.csAssertGuard #= 0
          dut.io.config.csDeassertGuard #= 0
          dut.io.config.csActiveState #= false
          dut.io.txData.valid #= true
          dut.io.txData.payload.last #= true
          dut.io.txData.payload.fragment #= 0x55
          dut.io.trigger.assert #= false
          dut.io.trigger.transfer #= false
          dut.io.trigger.deassert #= false

          dut.clockDomain.onActiveEdges {
            dut.io.spi.miso #= dut.io.spi.mosi.toBoolean
          }

          SimTimeout(10 * 8 * divider * toSend * 4)

          val scoreboard = ScoreboardInOrder[Int]()
          val monitorScoreboard = ScoreboardInOrder[Int]()
          val driver = StreamDriver(dut.io.txData, dut.clockDomain) { payload =>
            payload.fragment.randomize()
            payload.last #= (scoreboard.matches + scoreboard.ref.length >= toSend - 2)
            !(scoreboard.matches + scoreboard.ref.length > toSend - 2)
          }
          StreamMonitor(dut.io.txData, dut.clockDomain) { payload =>
            scoreboard.pushRef(payload.fragment.toInt)
            monitorScoreboard.pushRef(payload.fragment.toInt)
          }
          driver.delay = 0
          driver.transactionDelay = () => {
            0
          }
          FlowMonitor(dut.io.rxData, dut.clockDomain) { payload =>
            scoreboard.pushDut(payload.toInt)
          }

          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.waitActiveEdge(100)

          SpiMonitor(dut.io.spi, cpha, cpol, msbFirst = false, dut.clockDomain) { word =>
            monitorScoreboard.pushDut(word)
          }

          dut.io.trigger.transfer #= true
          dut.io.trigger.assert #= true
          dut.clockDomain.waitActiveEdge()
          dut.io.trigger.transfer #= false
          dut.io.trigger.assert #= false

          dut.clockDomain.waitActiveEdgeWhere(!dut.io.busy.toBoolean)
          dut.clockDomain.waitActiveEdge(1000)
          scoreboard.checkEmptyness()
          monitorScoreboard.checkEmptyness()
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
      val divider = 20
      SimTimeout(10 * 8 * divider * toSend * 4)
      val apb = Apb3Driver(dut.io.bus, dut.clockDomain)
      val scoreboard = ScoreboardInOrder[Int]()

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(100)

      dut.clockDomain.onActiveEdges {
        dut.io.spi.miso #= dut.io.spi.mosi.toBoolean
      }

      apb.write(0x18, divider << 2)

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
