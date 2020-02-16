package uhu01.basics

import spinal.core._
import spinal.sim._
import spinal.core.sim._


object PwmSim {
  def main(args: Array[String]) {
    SimConfig.withWave.workspacePath("/mnt/c/work/tmp/sim").doSim(new Pwm(3)) {dut =>
      dut.io.level #= 0
    
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitRisingEdge(100)

      dut.io.level #= 1
      dut.clockDomain.waitRisingEdge(100)

      dut.io.level #= (1 << 3) - 1
      dut.clockDomain.waitRisingEdge(100)
    }
  }
}