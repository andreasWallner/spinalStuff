package andreasWallner.io.pwm

import andreasWallner.SpinalFunSuite
import andreasWallner.io.pwm.sim._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver


class SlaveFifoMasterTest extends SpinalFunSuite {
  val dut = SimConfig.withWave
    .compile(
      Apb3Pwm(
        Pwm.PeripheralParameters(
          Pwm.CoreParameters(counterWidth = 10, channelCnt = 5),
          dividerWidth = 30
        )
      )
    )

  test(dut, "foo") { dut =>
    //SimTimeout(100000)

    val apb = Apb3Driver(dut.io.bus, dut.clockDomain)
    PwmDetect(dut.io.pwm(0), 2550, dut.clockDomain) {
      case PwmCycle(5000, 20600) =>
      case result => fail(f"invalid PWM output channel 0: $result")
    }
    PwmDetect(dut.io.pwm(1), 2550, dut.clockDomain) {
      case PwmCycle(10000, 15600) =>
      case result => fail(f"invalid PWM output channel 0: $result")
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
