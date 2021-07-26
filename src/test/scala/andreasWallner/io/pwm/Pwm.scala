package andreasWallner.io.pwm

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import andreasWallner.io.pwm._
import andreasWallner.io.pwm.sim._

import org.scalatest.FunSuite

class SlaveFifoMasterTest extends FunSuite {
  val dut = SimConfig.withWave
    .workspacePath("/c/work/tmp/sim")
    .compile(
      Apb3Pwm(
        Pwm.PeripheralParameters(
          Pwm.CoreParameters(counterWidth = 10, channelCnt = 5),
          dividerWidth = 30
        )
      )
    )

  test("foo") {
    dut.doSim("foo") { dut =>
      //SimTimeout(100000)
      
      val apb = Apb3Driver(dut.io.bus, dut.clockDomain)
      val decoder0 = PwmDetect(dut.io.pwm(0), 2550, dut.clockDomain) { result =>
        result match {
          case PwmCycle(5000, 20600) => 
          case _ => fail(f"invalid PWM output channel 0: ${result}")
        }
      }
      val decoder1 = PwmDetect(dut.io.pwm(1), 2550, dut.clockDomain) { result =>
        result match {
          case PwmCycle(10000, 15600) => 
          case _ => fail(f"invalid PWM output channel 0: ${result}")
        }
      }
      
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)
      apb.write(4, 9)
      apb.write(8, 255)
      apb.write(12, 50)
      apb.write(16, 100)
      apb.write(0, 1)

      dut.clockDomain.waitActiveEdge(50000)
    }
  }
}
