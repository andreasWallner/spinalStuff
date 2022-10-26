package andreasWallner.iceblink

import andreasWallner.SpinalFunSuite
import andreasWallner.iceblink.sim.EPPSimMaster
import andreasWallner.sim._
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class EppLaDemoTest extends SpinalFunSuite {
  val dut = SimConfig
    .withWaveOverride("fst")
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(3.33 MHz))
    )
    .compile(EppLaDemo())

  test(dut, "read and write") { dut =>
    //SimTimeout(500000)
    val es = EPPSimMaster(dut.io.epp, 10)
    dut.io.epp.DB.simulatePullup()
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling()
    sleep(500)

    es.write(0x00, 0xff)
    sleep(100)
    es.write(0x01, 0x55)
    sleep(100)
    es.read(0x01)

    sleep(400000)
  }
}
