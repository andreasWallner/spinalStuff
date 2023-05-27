package andreasWallner.io.spi

import spinal.core.sim._
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import andreasWallner.io.spi.sim._
import spinal.core.IntToBuilder
import spinal.lib.sim.{FlowMonitor, StreamDriver, StreamMonitor}

import scala.language.postfixOps
import scala.util.Random

class SpiSlaveControllerTest extends SpinalFunSuite {
  val dut = namedSimConfig.compile(SpiSlaveController(8 bit))

  for (cpol <- Seq(true, false); cpha <- Seq(true, false)) {
    val configStr = s" CPHA $cpha CPOL $cpol"

    test(dut, "normal" + configStr) { dut =>
      dut.io.config.cpha #= cpha
      dut.io.config.cpol #= cpol
      dut.io.en #= true

      val txScoreboard = LoggingScoreboardInOrder[Int]("tx", (i: Int) => f"$i%02x")
      val rxScoreboard = LoggingScoreboardInOrder[Int]("rx", (i: Int) => f"$i%02x")
      val driver = new SpiDriver(dut.io.spi, cpha, cpol, false, 10, 5, 5, dut.clockDomain)
      StreamDriver(dut.io.tx, dut.clockDomain) { p =>
        p.randomize(); true
      }.transactionDelay = () => 0
      StreamMonitor(dut.io.tx, dut.clockDomain) { p =>
        txScoreboard.pushRef(p.toInt)
      }
      FlowMonitor(dut.io.rx, dut.clockDomain) { p =>
        rxScoreboard.pushDut(p.toInt)
      }

      SpiSlaveMonitor(
        dut.io.spi,
        cpha = cpha,
        cpol = cpol,
        msbFirst = false,
        dut.clockDomain
      ) { i =>
        txScoreboard.pushDut(i)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(50)

      for (_ <- 0 until 20) {
        val words = Seq.fill(Random.nextInt(20) + 1)(Random.nextInt(255))
        words.foreach(rxScoreboard.pushRef)
        driver.write(words)
        dut.clockDomain.waitSampling(5)
      }

      sleep(1000)
      txScoreboard.checkEmptyness()
      rxScoreboard.checkEmptyness()
    }

    test(dut, "polling" + configStr) { dut =>
      val pollBytes = 9
      dut.io.config.cpha #= cpha
      dut.io.config.cpol #= cpol
      dut.io.tx.valid #= false
      dut.io.en #= true

      val txScoreboard = LoggingScoreboardInOrder[Int]("tx", (i: Int) => f"$i%02x")
      val rxScoreboard = LoggingScoreboardInOrder[Int]("rx", (i: Int) => f"$i%02x")
      val driver = new SpiDriver(dut.io.spi, cpha, cpol, false, 10, 5, 5, dut.clockDomain)
      FlowMonitor(dut.io.rx, dut.clockDomain) { p =>
        rxScoreboard.pushDut(p.toInt)
      }

      SpiSlaveMonitor(
        dut.io.spi,
        cpha = cpha,
        cpol = cpol,
        msbFirst = false,
        dut.clockDomain
      ) { i =>
        txScoreboard.pushDut(i)
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(50)

      val sender = fork {
        val words = Seq.fill(pollBytes + 1)(Random.nextInt(255))
        words.foreach(rxScoreboard.pushRef)
        driver.write(words)
        dut.clockDomain.waitSampling(5)
      }

      for (_ <- 0 until pollBytes - 1) {
        txScoreboard.pushRef(0)
        waitUntil(dut.io.status.busy.toBoolean)
        waitUntil(!dut.io.status.busy.toBoolean)
      }
      for( _ <- 0 until 5) {
        waitUntil(dut.io.spi.sclk.toBoolean)
        waitUntil(!dut.io.spi.sclk.toBoolean)
      }
      txScoreboard.pushRef(0)
      dut.io.tx.valid #= true
      val expected = dut.io.tx.payload.randomize()
      txScoreboard.pushRef(expected.toInt)
      waitUntil(dut.io.tx.ready.toBoolean)
      dut.io.tx.payload.randomize()

      waitUntil(sender.isDone)

      sleep(1000)
      txScoreboard.checkEmptyness()
      rxScoreboard.checkEmptyness()
    }
  }
}
