package andreasWallner.io.iso7816

import andreasWallner.PayloadRandmizer
import andreasWallner.io.iso7816.sim._
import andreasWallner.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, StreamDriver, StreamMonitor}

import scala.util.Random

class ISO7816MasterSim extends AnyFunSuite {
  val dut = SimConfig.withFstWave
    .compile(ISO7816Master(CoreGenerics()))

  test("TXRX_cycle") {
    dut.doSim("TXRX_cycle") { dut =>
      val toSend = 10
      SimTimeout(toSend * 400 * 10 * 16)
      dut.io.config.control.ta #= 200
      dut.io.config.control.tb #= 300
      dut.io.config.control.te #= 300
      dut.io.config.control.th #= 300
      dut.io.config.control.clk_offset #= 20
      dut.io.config.control.vcc_offset #= 10
      dut.io.config.rxtx.characterRepetition #= false
      dut.io.config.rxtx.baudrateDivider #= 40
      dut.io.config.rxtx.cgt #= 10
      dut.io.config.clockrate #= 20
      dut.io.config.bwt #= 1000
      dut.io.config.cwt #= 100

      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.io.start.deactivate #= false
      dut.io.start.activate #= false
      dut.io.start.reset #= false
      dut.io.start.stop_clock #= false

      dut.clockDomain.forkStimulus(10)

      dut.io.iso.io.simulatePullup()
      dut.io.iso.io.prohibitAnyConcurrentDrivers()

      val rxScoreboard = ScoreboardInOrder[Int]()
      val txScoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.tx, dut.clockDomain)(PayloadRandmizer(toSend).apply)
      StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
        rxScoreboard.pushRef(payload.toInt)
      }
      FlowMonitor(dut.io.rx, dut.clockDomain) { payload =>
        //simLog(s"tx dut ${payload.toInt}")
        txScoreboard.pushDut(payload.toInt)
      }

      dut.io.start.activate.strobe(dut.clockDomain)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.state.change_active.toBoolean)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.change_active.toBoolean)
      dut.clockDomain.waitSampling(100)

      val rx = ISO7816SimRx(dut.io.iso.io, 400) { (data, parityValid) =>
        assert(parityValid)
        rxScoreboard.pushDut(data)
        true
      }
      val tx = ISO7816SimTx(dut.io.iso.io, 400) { (data, error, inducedError) =>
        assert(!error)
        assert(!inducedError)
        txScoreboard.pushRef(data)
        //simLog(s"tx ref $data")
      }

      dut.clockDomain.waitActiveEdgeWhere(dut.io.tx.valid.toBoolean)
      dut.io.start.tx.strobe(dut.clockDomain)
      dut.io.start.rx.strobe(dut.clockDomain)
      dut.clockDomain.waitActiveEdgeWhere(dut.io.state.tx_active.toBoolean)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.tx_active.toBoolean)

      rx.disableRx = true
      sleep(5000)
      simLog("starting to transmit")
      tx.tx("1122334455")

      dut.clockDomain.waitSamplingWhere(!dut.io.state.rx_active.toBoolean)

      rxScoreboard.checkEmptyness()
      txScoreboard.checkEmptyness()
      assert(rxScoreboard.matches == toSend)
      assert(txScoreboard.matches == 5)
    }
  }
}
