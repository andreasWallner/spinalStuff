package andreasWallner.io.iso7816

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class TestInactiveState extends AnyFunSuite {
  import Helper._
  val dut = SimConfig.withWave.compile(StateCtrl())

  test("Initial State") {
    dut.doSim("Initial State") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      verify_inactive(dut)
    }
  }

  test("Inactive | Activate -> Active") {
    dut.doSim("Inactive Activate Active") { dut =>
      SimTimeout(10000)
      zero_io(dut)
      dut.io.config.ta #= 50
      dut.io.config.tb #= 120
      dut.io.config.vcc_offset #= 10

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdge(400)

      verify_active(dut)
    }
  }
}

class TestActiveState extends AnyFunSuite {
  import Helper._
  val dut = SimConfig.withWave.compile(StateCtrl())

  test("Active | Deactivate -> Inactive") {
    dut.doSim("active deactivate inactive") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Deactivate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_inactive(dut)
    }
  }
  test("Active | StopClock -> Clockstop") {
    dut.doSim("active stopclock clockstop") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.StopClock)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_clockstop(dut)
    }
  }
  test("Active | Reset -> Reset") {
    dut.doSim("active reset reset") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.GotoReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      verify_reset(dut)
    }
  }
  test("Active | WarmReset -> Active") {
    dut.doSim("active warmreset active") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      dut.io.config.te #= 10
      dut.io.config.ta #= 100
      dut.io.config.tb #= 200

      send_command(dut, CtrlCommand.WarmReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      verify_active(dut)
    }
  }
  test("Active | ColdReset -> Active") {
    dut.doSim("active coldreset active") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      dut.io.config.te #= 10
      dut.io.config.vcc_offset #= 30
      dut.io.config.th #= 100
      dut.io.config.clk_offset #= 10
      dut.io.config.ta #= 100
      dut.io.config.tb #= 200

      send_command(dut, CtrlCommand.ColdReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)

      verify_active(dut)
    }
  }
}

class TestResetState extends AnyFunSuite {
  import Helper._
  val dut = SimConfig.withWave.compile(StateCtrl())

  test("Reset | Activate -> Active") {
    dut.doSim("active deactivate inactive") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      send_command(dut, CtrlCommand.GotoReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(count = 5)

      dut.io.config.ta #= 20
      dut.io.config.tb #= 40

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_active(dut)
    }
  }
}

class TestClockStopState extends AnyFunSuite {
  import Helper._
  val dut = SimConfig.withWave.compile(StateCtrl())

  def goto_clockstop(dut: StateCtrl): Unit = {
    send_command(dut, CtrlCommand.Activate)
    dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
    dut.clockDomain.waitActiveEdge(5)

    send_command(dut, CtrlCommand.StopClock)
    dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
    dut.clockDomain.waitActiveEdge(5)
  }

  test("ClockStop | Activate -> Active") {
    dut.doSim("active deactivate inactive") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      goto_clockstop(dut)

      dut.io.config.clk_offset #= 40

      send_command(dut, CtrlCommand.Activate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_active(dut)
    }
  }

  test("ClockStop | Reset -> Reset") {
    dut.doSim("clockstop reset reset") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      goto_clockstop(dut)

      dut.io.config.te #= 40

      send_command(dut, CtrlCommand.GotoReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_reset(dut)
    }
  }

  test("ClockStop | Deactivate -> Inactive") {
    dut.doSim("clockstop deactivate inactive") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      goto_clockstop(dut)

      dut.io.config.te #= 10
      dut.io.config.vcc_offset #= 40

      send_command(dut, CtrlCommand.Deactivate)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_inactive(dut)
    }
  }

  test("ClockStop | ColdReset -> Active") {
    dut.doSim("clockstop coldreset active") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      goto_clockstop(dut)

      dut.io.config.te #= 10
      dut.io.config.vcc_offset #= 30
      dut.io.config.th #= 200
      dut.io.config.clk_offset #= 10
      dut.io.config.ta #= 40
      dut.io.config.tb #= 60

      send_command(dut, CtrlCommand.ColdReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_active(dut)
    }
  }

  test("ClockStop | WarmReset -> Active") {
    dut.doSim("clockstop warmreset active") { dut =>
      SimTimeout(10000)
      zero_io(dut)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(5)

      goto_clockstop(dut)

      dut.io.config.te #= 10
      dut.io.config.th #= 100
      dut.io.config.clk_offset #= 5
      dut.io.config.ta #= 20
      dut.io.config.tb #= 30

      send_command(dut, CtrlCommand.WarmReset)
      dut.clockDomain.waitActiveEdgeWhere(!dut.io.state.busy.toBoolean)
      dut.clockDomain.waitActiveEdge(5)
      verify_active(dut)
    }
  }
}


object Helper {
  def zero_io(dut: StateCtrl): Unit = {
    dut.io.iso.io.read #= false
    dut.io.config.ta #= 0
    dut.io.config.tb #= 0
    dut.io.config.te #= 0
    dut.io.config.th #= 0
    dut.io.config.clk_offset #= 0
    dut.io.config.vcc_offset #= 0
    dut.io.tx_done #= false
    dut.io.command.valid #= false
  }

  def send_command(
                    dut: StateCtrl,
                    cmd: SpinalEnumElement[CtrlCommand.type]
  ): Unit = {
    dut.io.command.payload #= cmd
    dut.io.command.valid #= true
    dut.clockDomain.waitActiveEdge()
    dut.io.command.valid #= false
  }

  def verify_inactive(dut: StateCtrl): Unit = {
    assert(!dut.io.iso.vcc.toBoolean)
    assert(!dut.io.iso.rst.toBoolean)
    assert(!dut.io.state.clock.toBoolean)

    assert(dut.io.state.driving_io.toBoolean)
    assert(dut.io.iso.io.writeEnable.toBoolean)
    assert(!dut.io.iso.io.write.toBoolean)
  }

  def verify_active(dut: StateCtrl): Unit = {
    assert(dut.io.iso.vcc.toBoolean)
    assert(dut.io.iso.rst.toBoolean)
    assert(dut.io.state.clock.toBoolean)

    assert(!dut.io.state.driving_io.toBoolean)
  }

  def verify_reset(dut: StateCtrl): Unit = {
    assert(dut.io.iso.vcc.toBoolean)
    assert(!dut.io.iso.rst.toBoolean)
    assert(dut.io.state.clock.toBoolean)

    assert(dut.io.state.driving_io.toBoolean)
    assert(!dut.io.iso.io.writeEnable.toBoolean)
  }

  def verify_clockstop(dut: StateCtrl): Unit = {
    assert(dut.io.iso.vcc.toBoolean)
    assert(dut.io.iso.rst.toBoolean)
    assert(!dut.io.state.clock.toBoolean)

    assert(dut.io.state.driving_io.toBoolean)
    assert(!dut.io.iso.io.writeEnable.toBoolean)
  }
}
