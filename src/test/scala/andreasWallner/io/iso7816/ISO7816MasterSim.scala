package andreasWallner.io.iso7816

import andreasWallner.io.iso7816.sim._
import andreasWallner.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.lib.sim.{
  FlowMonitor,
  ScoreboardInOrder,
  StreamDriver,
  StreamMonitor
}

import scala.util.Random

class ISO7816MasterSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .compile(ISO7816Master(CoreGenerics()))

  test("TXRX_cycle") {
    dut.doSim("TXRX_cycle") { dut =>
      val toSend = 500
      SimTimeout(toSend * 10 * 8 * 10 * 2 * 2)
      dut.io.config.control.ta #= 200
      dut.io.config.control.tb #= 300
      dut.io.config.control.te #= 300
      dut.io.config.control.th #= 300
      dut.io.config.control.clk_offset #= 20
      dut.io.config.control.vcc_offset #= 10
      dut.io.config.rxtx.characterRepetition #= false
      dut.io.config.rxtx.baudrateDivider #= 40
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
      //dut.io.iso.io.prohibitAnyConcurrentDrivers()

      val scoreboard = ScoreboardInOrder[Int]()
      StreamDriver(dut.io.tx, dut.clockDomain) { payload =>
        payload.randomize()
        scoreboard.matches + scoreboard.ref.size < toSend
      }
      StreamMonitor(dut.io.tx, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload.toInt)
      }
      ISO7816SimRx(dut.io.iso.io, 100) { (data, parityValid) =>
        assert(parityValid)
        scoreboard.pushDut(data)
        true
      }

      dut.clockDomain.waitActiveEdgeWhere(dut.io.tx.valid.toBoolean)
      dut.io.start.tx #= true
      dut.io.start.rx #= true
      dut.clockDomain.waitActiveEdge(1)
      dut.io.start.tx #= false
      dut.io.start.rx #= false
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.tx_active.toBoolean)



      scoreboard.checkEmptyness()
      assert(scoreboard.matches == toSend)
    }
  }
}
