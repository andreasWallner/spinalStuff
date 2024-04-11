package andreasWallner.io.iso7816

import andreasWallner.SpinalFunSuite
import spinal.core.sim._

class S10ManualTest extends SpinalFunSuite {
  val dut = namedSimConfig.compile(S10Master(16, 10))
  test(dut, "blub") { dut =>
    dut.io.cmd.valid #= false
    dut.io.timings.por #= 50
    dut.io.timings.low_clk #= 5
    dut.io.timings.post_write_low_clk #= 15
    dut.io.timings.high_clk_en #= 2
    dut.io.timings.high_clk_rem #= 3
    dut.io.timings.rst_pre_post #= 7
    dut.io.timings.rst #= 22
    dut.io.timings.write_pulse_pre_post #= 11
    dut.io.timings.write_pulse #= 8
    dut.io.timings.write #= 40
    dut.io.timings.dummy #= 30
    dut.io.iso.data.read #= false

    dut.clockDomain.forkStimulus(10)
    dut.io.cmd.payload #= Action.vcc_on
    dut.io.cmd.valid #= true
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.reset
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.clock
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.dummyWrite
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.transmit1
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.transmit0
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.transmit1
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.transmit1
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.valid #= false

    dut.clockDomain.waitSampling(50)

    dut.io.cmd.payload #= Action.clock
    dut.io.cmd.valid #= true
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)

    dut.io.cmd.payload #= Action.write
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.payload #= Action.clock
    dut.clockDomain.waitSamplingWhere(dut.io.cmd.ready.toBoolean)
    dut.io.cmd.valid #= false

    dut.clockDomain.waitSampling(50)

  }
}

class S10PeripheralManualTest extends SpinalFunSuite {
  import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
  import spinal.lib.bus.amba3.apb.sim.Apb3Driver

  val apbConfig = Apb3Config(addressWidth = 5, dataWidth = 8, useSlaveError = false)
  val dut = namedSimConfig.withVcdWave.compile(S10MasterPeripheral[Apb3](2, Apb3(apbConfig), Apb3SlaveFactory(_)))

  test(dut, "sanity check") { dut =>
    val driver = Apb3Driver(dut.io.bus, dut.clockDomain)
    dut.clockDomain.forkStimulus(10)
    dut.io.iso.data.read #= true

    fork {
      dut.clockDomain.waitSamplingWhere(dut.io.iso.rst.toBoolean)
      dut.clockDomain.waitSamplingWhere(!dut.io.iso.rst.toBoolean)

      for(b <- Seq(false, false, false, false, true, true, true, true, true, true, true, false, true, true, true, false, true, false, true, false, true, false, true, false)) {
        dut.io.iso.data.read #= b
        dut.clockDomain.waitSamplingWhere(dut.io.iso.clk.toBoolean)
        dut.clockDomain.waitSamplingWhere(!dut.io.iso.clk.toBoolean)
      }
    }

    driver.write(2, 20)   // por_lo
    driver.write(3, 0)    // por_hi
    driver.write(4, 15)   // low_clk
    driver.write(5, 40)   // post_write_low_clk
    driver.write(6, 5)    // high_clk_en
    driver.write(7, 10)   // high_clk_ram
    driver.write(8, 20)   // rst_pre_post
    driver.write(9, 0)    // rst_lo
    driver.write(10, 1)   // rst_hi
    driver.write(11, 20)  // write_pulse_pre_post
    driver.write(12, 20)  // write_pulse
    driver.write(13, 150) // write_hi
    driver.write(14, 4)   // write_lo
    driver.write(15, 3)   // write_cnt
    driver.write(16, 50)  // dummy write
    driver.write(18, 50)   // por cnt

    driver.write(17, 0)
    driver.write(17, 1)
    for(i <- 0 until 3*8)
      driver.write(17, 7)
    driver.write(17, 3)
    driver.write(17, 3)
    for (i <- 0 until 10)
      driver.write(17, 2)
    driver.write(17, 5)
    driver.write(17, 5)
    driver.write(17, 6)
    driver.write(17, 5)
    driver.write(17, 6)

    driver.write(17, 2)
    driver.write(17, 2)

    driver.write(17, 0)
    driver.write(17, 2)
    driver.write(17, 2)

    driver.write(0, 0x6)

    // TODO rx test

    dut.clockDomain.waitSampling(10)
    dut.clockDomain.waitSamplingWhere(!dut.io.busy.toBoolean)
    dut.clockDomain.waitSampling(10)

    while ((driver.read(1) & 1) != 0) {}
    driver.write(0, 0x08)

    assert(driver.read(18) == 0xf0)
    assert(driver.read(18) == 0x77)
    assert(driver.read(18) == 0x55)
  }
}
