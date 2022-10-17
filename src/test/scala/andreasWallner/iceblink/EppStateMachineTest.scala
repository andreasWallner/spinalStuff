package andreasWallner.iceblink

import andreasWallner.SpinalFunSuite
import andreasWallner.iceblink.sim.EPPSimMaster
import andreasWallner.sim._
import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.slave

import scala.util.Random

class EppStateMachineTest extends AnyFunSuite {
  val dut = SimConfig.withFstWave
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    )
    .compile(EPPStateMachine())

  test("read and write") {
    dut.doSim("read and write") { dut =>
      SimTimeout(1000)
      val es = EPPSimMaster(dut.io.epp, 10)
      dut.io.epp.DB.simulatePullup()

      dut.clockDomain.forkStimulus(10)

      dut.clockDomain.waitSampling()
      sleep(100 + 10 / 2)

      assert(es.read(0x00) == 0x55)
      assert(es.read(0x01) == 0x77)

      es.write(0x00, 0x81)
      assert(dut.io.reg0.toInt == 0x81)

      es.write(0x01, 0x7e)
      assert(dut.io.reg1.toInt == 0x7e)
    }
  }
}

case class EppBusSlaveFactoryTester() extends Component {
  val io = new Bundle {
    val epp = slave(EPP())
    val writeReg = out Bits (8 bit)
    val readReg = in Bits (8 bit)
  }

  val factory = new EppBusFactory(io.epp)
  io.writeReg := factory.createReadAndWrite(Bits(8 bit), 0, 0)
  factory.read(io.readReg, 1, 0)
}

class EppBusSlaveFactoryTest extends SpinalFunSuite {
  val dut = SimConfig.withFstWave
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    )
    .compile(EppBusSlaveFactoryTester())

  test(dut, "read and write") { dut =>
    SimTimeout(10000)
    val es = EPPSimMaster(dut.io.epp, 10)
    dut.io.epp.DB.simulatePullup()

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSampling()
    sleep(100 + 10 / 2)

    for (_ <- 1 to 5) {
      val readVal = Random.nextInt(256)
      dut.io.readReg #= readVal
      println(f"read randomized ${readVal}")
      assert(es.read(0x01) == dut.io.readReg.toInt)

      val writeVal = Random.nextInt(256)
      println(f"write randomized ${writeVal}")
      es.write(0x00, writeVal)
      assert(dut.io.writeReg.toInt == writeVal)
      assert(es.read(0x00) == writeVal)
    }

  }
}
