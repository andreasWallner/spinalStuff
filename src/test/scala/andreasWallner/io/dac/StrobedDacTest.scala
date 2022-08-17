package andreasWallner.io.dac

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import org.scalatest.funsuite.AnyFunSuite

case class Apb3Dac(
    g: StrobedDacGenerics = StrobedDacGenerics(),
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends StrobedDacPeripheral[Apb3](
      g,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    )

class StrobedDacPeripheralTest extends AnyFunSuite {
  val dut = SimConfig.withWave
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    )
    .compile(Apb3Dac())

  test("write 2") {
    dut.doSim("write 2") { dut =>
      SimTimeout(1000)
      val apb = Apb3Driver(dut.io.bus, dut.clockDomain)

      dut.clockDomain.forkStimulus(10)

      // wait until module is idle
      while (apb.read(0) != 0) {}

      apb.write(4, 0x22)
      while (apb.read(0) != 0) {}

      apb.write(4, 0x80000044L)
      while (apb.read(0) != 0) {}

      // TODO automatic verification
    }
  }
}
