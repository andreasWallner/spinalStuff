package andreasWallner.io.ftdi

import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import andreasWallner.io.ftdi.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.language.postfixOps
import scala.util.Random

class AsyncFifoControllerTest extends SpinalFunSuite {
  val frequency = 100 MHz
  val dut = namedSimConfig
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(frequency))
    )
    .compile(AsyncFifoController())

  test(dut, "TX only") { dut =>
    SimTimeout((100 MHz).toTime * 1000 * 100)

    dut.io.rx.ready #= true
    dut.clockDomain.forkStimulus(frequency.toTime)

    val scoreboard = ScoreboardInOrder[Int]()
    new AsyncFifoDriver(dut.io.fifo, dut.clockDomain) {
      override def tx(bits: Int): Unit = scoreboard.pushDut(bits)
    }
    StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
      payload.randomize()
      true
    }.transactionDelay = () => Random.nextInt(100)
    StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
      scoreboard.pushRef(payload.toInt)
    }

    waitUntil(scoreboard.matches == 1000)
  }

  test(dut, "RX only") { dut =>
    SimTimeout((100 MHz).toTime * 100000 * 100)

    dut.io.tx.valid #= false
    dut.clockDomain.forkStimulus(frequency.toTime)

    val scoreboard = ScoreboardInOrder[Int]()
    new AsyncFifoDriver(dut.io.fifo, dut.clockDomain) {
      override def doRx() = (Random.nextInt(100) < 2, Random.nextInt(255))

      override def rx(bits: Int): Unit = scoreboard.pushRef(bits)
    }
    StreamReadyRandomizer(dut.io.rx, dut.clockDomain)
    StreamMonitor(dut.io.rx, dut.clockDomain) { payload =>
      scoreboard.pushDut(payload.toInt)
    }

    waitUntil(scoreboard.matches == 1000)
  }

  test(dut, "RX and TX") { dut =>
    SimTimeout((100 MHz).toTime * 100000 * 100)

    dut.io.tx.valid #= false
    dut.clockDomain.forkStimulus(frequency.toTime)

    val rxScoreboard = ScoreboardInOrder[Int]()
    val txScoreboard = ScoreboardInOrder[Int]()
    new AsyncFifoDriver(dut.io.fifo, dut.clockDomain) {
      override def doRx() = (Random.nextInt(100) < 2, Random.nextInt(255))
      override def rx(bits: Int): Unit = rxScoreboard.pushRef(bits)
      override def tx(bits: Int): Unit = txScoreboard.pushDut(bits)
    }
    StreamReadyRandomizer(dut.io.rx, dut.clockDomain)
    StreamMonitor(dut.io.rx, dut.clockDomain) { payload =>
      rxScoreboard.pushDut(payload.toInt)
    }

    StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
      payload.randomize()
      true
    }.transactionDelay = () => Random.nextInt(100)
    StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
      txScoreboard.pushRef(payload.toInt)
    }

    waitUntil(rxScoreboard.matches > 1000 && txScoreboard.matches > 1000)
  }
}
