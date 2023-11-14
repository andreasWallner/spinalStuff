package andreasWallner.demos

import spinal.core._
import spinal.core.sim._
import andreasWallner.SpinalFunSuite

import scala.language.postfixOps

class ArtyEthFloodTest() extends SpinalFunSuite {
  val dut = namedSimConfig.withConfig(
    SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
  ) compile (new ArtyEthFlood())

  test(dut, "test") { dut =>
    dut.io.ethernet.rxClockDomain.forkStimulus((25 MHz).toTime)
    dut.io.ethernet.txClockDomain.forkStimulus((25 MHz).toTime)
    dut.clockDomain.forkStimulus((100 MHz).toTime)

    sleep(100000000)
  }
}
