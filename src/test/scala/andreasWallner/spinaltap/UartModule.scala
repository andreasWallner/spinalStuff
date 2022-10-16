package andreasWallner.spinaltap

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim._

import org.scalatest.funsuite.AnyFunSuite

class UartModuleSim extends AnyFunSuite {
  val dut = SimConfig.withWave
    .compile(UartModule(1))

  /*test("txrx") {
    dut.doSim("txrx") { dut =>
      val toSend = 5
      SimTimeout(500000)

      dut.clockDomain.onActiveEdges {
        dut.io.uart.rxd #= dut.io.uart.txd.toBoolean
      }

      dut.io.data.valid #= false
      dut.io.clockDivider #= 4

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge()
      dut.io.data.payload #= 1
      dut.io.data.valid #= true
      dut.clockDomain.waitActiveEdge()
      dut.io.data.payload #= 2
      dut.clockDomain.waitActiveEdge()
      dut.io.data.payload #= 3
      dut.clockDomain.waitActiveEdge()
      dut.io.data.payload #= 4
      dut.clockDomain.waitActiveEdge()
      dut.io.data.payload #= 5
      dut.clockDomain.waitActiveEdge()
      dut.io.data.valid #= false

      dut.clockDomain.waitActiveEdge(5000)
    }
  }*/
}
