package andreasWallner.io.pwm

import andreasWallner.SpinalFunSuite
import andreasWallner.io.pwm.sim._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver


class PwmPeripheralTest extends SpinalFunSuite {
  val dut = SimConfig.withWave
    .compile(
      Apb3Pwm(
        Pwm.PeripheralParameters(
          Pwm.CoreParameters(counterWidth = 10, channelCnt = 5, modes=List(Pwm.OutputMode.SET_RESET)),
          dividerWidth = 30
        )
      )
    )

  test(dut, "set_reset, normal config") { dut =>
    // configure divider (4) to 9 -> divides clock by 10
    //           max_count (8) to 255
    //           pwm0 (12) to 50
    //           pwm1 (16) to 100
    //           pwm2 (20) to 256
    // all times are taken x 10 for divider
    // and                 x 10 for sim cycle duration

    val apb = Apb3Driver(dut.io.bus, dut.clockDomain)
    PwmDetect(dut.io.pwm(0), 2550, dut.clockDomain) {
      case PwmCycle(50_00, 206_00) =>
      case result => fail(f"invalid PWM output channel 0: $result")
    }
    PwmDetect(dut.io.pwm(1), 2550, dut.clockDomain) {
      case PwmCycle(100_00, 156_00) =>
      case result => fail(f"invalid PWM output channel 1: $result")
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitActiveEdge(5)
    apb.write(4, 9)
    apb.write(8, 255)
    apb.write(12, 50)
    apb.write(16, 100)
    apb.write(20, 256)
    apb.write(0, 1)

    dut.clockDomain.waitActiveEdge(50000)
  }
}
