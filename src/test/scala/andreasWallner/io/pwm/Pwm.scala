package andreasWallner.io.pwm

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import andreasWallner.io.pwm._
import andreasWallner.io.pwm.sim._

import org.scalatest.FunSuite

case class ApbSim(apb3: Apb3, clockDomain: ClockDomain) {
  def write(address: BigInt, value: BigInt, sel: Int = 1) {
    apb3.PSEL #= 0
    clockDomain.waitActiveEdge()
    apb3.PSEL(sel) #= true
    clockDomain.waitActiveEdge()
    apb3.PADDR #= address
    clockDomain.waitActiveEdge()
    apb3.PWRITE #= true
    clockDomain.waitActiveEdge()
    apb3.PWDATA #= value
    clockDomain.waitActiveEdge()

    apb3.PENABLE #= true
    clockDomain.waitActiveEdgeWhere(apb3.PREADY.toBoolean)
    apb3.PSEL #= 0
    apb3.PENABLE #= false
  }
}

class SlaveFifoMasterTest extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(
      Apb3Pwm(
        Pwm.PeripheralParameters(
          Pwm.CoreParameters(counterWidth = 10, channelCnt = 5),
          prescalerWidth = 30
        )
      )
    )

  test("foo") {
    dut.doSim("foo") { dut =>
      SimTimeout(10000)

      val apb = ApbSim(dut.io.bus, dut.clockDomain)
      
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)
      apb.write(0, 9)

      dut.clockDomain.waitActiveEdge(500)
    }
  }
}
