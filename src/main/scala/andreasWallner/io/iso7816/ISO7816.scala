package andreasWallner.io.iso7816

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState

case class ISO7816() extends Bundle with IMasterSlave {
  val io = TriState(Bool)
  val vcc = Bool
  val rst = Bool
  val clk = Bool

  def asMaster() = {
    master(io)
    out(vcc, rst, clk)
  }
}

case class ISO7816MasterConfig() extends Bundle {
  val characterRepetition = Bool()
  val cgt = UInt(8 bit) // in ETUs - 1 // TODO: size correctly,
}

// TODO: seperate timeouts for receive and reset?

object CtrlState extends SpinalEnum {
  val Inactive, Active, Reset, ClockStop = newElement()
}

object CtrlCommand extends SpinalEnum {
  val Idle, Deactivate, Activate, Reset, StopClock, ColdReset, WarmReset =
    newElement()
}

case class ISO7816StateCtrl() extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val vcc = out Bool
      val rst = out Bool
      val io = master(TriState(Bool))
    }
    val config = new Bundle {
      val ta = in UInt (32 bit)
      val tb = in UInt (32 bit)
      val te = in UInt (32 bit)
      val th = in UInt (32 bit)
      val clk_offset = in UInt (32 bit)
      val vcc_offset = in UInt (32 bit)
    }
    val command = slave(Flow(CtrlCommand()))
    val start_rx = out Bool
    val tx_done = in Bool // strobed on TX for clkstop min time
    val inhibit_tx = out Bool
    val state = new Bundle {
      val driving_io = out Bool
      val clock = out Bool
      val busy = out Bool
    }
  }

  val state = Reg(CtrlState()).init(CtrlState.Inactive)
  val command = Reg(CtrlCommand()).init(CtrlCommand.Idle)
  val active = command =/= CtrlCommand.Idle
  io.state.busy := active
  val timing = new Area {
    val reset = RegNext(state) =/= state
    val count = Reg(UInt(32 bit))
    when(reset) {
      count := 0
    } elsewhen (active) {
      count := count + 1
    }
    val ta_reached = count >= io.config.ta
    val tb_reached = count >= io.config.tb
    val te_reached = count >= io.config.te
    val th_reached = count >= io.config.th
    val clk_offset_reached = count >= io.config.clk_offset
    val vcc_offset_reached = count >= io.config.vcc_offset
  }

  when(!active) {
    timing.reset := True
    when(io.command.valid) {
      command := io.command.payload
    }
  }

  switch(state) {
    is(CtrlState.Inactive) {
      io.iso.vcc := False
      io.iso.rst := False
      io.state.clock := False
      io.state.driving_io := True
      io.iso.io.writeEnable := True
      io.iso.io.write := False
    }
    is(CtrlState.Active) {
      io.iso.vcc := True
      io.iso.rst := True
      io.state.clock := True
      io.state.driving_io := False
      io.iso.io.writeEnable := True
      io.iso.io.write := True
    }
    is(CtrlState.ClockStop) {
      io.iso.vcc := True
      io.iso.rst := True
      io.state.clock := False
      io.state.driving_io := True
      io.iso.io.writeEnable := False
      io.iso.io.write := True
    }
    is(CtrlState.Reset) {
      io.iso.vcc := True
      io.iso.rst := False
      io.state.clock := True
      io.state.driving_io := True
      io.iso.io.writeEnable := False
      io.iso.io.write := False
    }
  }

  io.start_rx := False
  io.inhibit_tx := False
  when(active) {
    switch(state) {
      is(CtrlState.Inactive) {
        switch(command) {
          is(CtrlCommand.Activate, CtrlCommand.ColdReset) {
            io.iso.vcc := True
            io.state.clock := timing.vcc_offset_reached
            io.iso.io.write := timing.ta_reached
            io.iso.rst := timing.tb_reached
            when(
              timing.clk_offset_reached && timing.ta_reached && timing.tb_reached
            ) {
              state := CtrlState.Active
              command := CtrlCommand.Idle
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }

      is(CtrlState.Active) {
        switch(command) {
          is(CtrlCommand.Deactivate, CtrlCommand.ColdReset) {
            io.state.driving_io := True
            io.iso.vcc := !timing.vcc_offset_reached
            io.state.clock := False
            io.iso.rst := False
            io.iso.io.writeEnable := timing.te_reached
            io.iso.io.write := False
            when(
              timing.vcc_offset_reached && timing.te_reached && (command =/= CtrlCommand.ColdReset || timing.th_reached)
            ) {
              state := CtrlState.Inactive
            }
          }

          is(CtrlCommand.StopClock) {
            when(!io.tx_done) { // TODO time
              state := CtrlState.ClockStop
            }
          }

          is(CtrlCommand.Reset, CtrlCommand.WarmReset) {
            io.state.driving_io := True
            io.iso.io.writeEnable := False
            when(timing.te_reached) {
              state := CtrlState.Reset
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }

      is(CtrlState.Reset) {
        switch(command) {
          is(CtrlCommand.Activate, CtrlCommand.WarmReset) {
            io.iso.rst := !timing.tb_reached
            io.iso.io.writeEnable := !timing.ta_reached
            when(timing.ta_reached && timing.tb_reached) {
              state := CtrlState.Active
              command := CtrlCommand.Idle
              io.start_rx := True
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }

      is(CtrlState.ClockStop) {
        switch(command) {
          is(CtrlCommand.Deactivate, CtrlCommand.ColdReset) {
            io.iso.vcc := !timing.vcc_offset_reached
            io.iso.io.write := False
            io.iso.io.writeEnable := timing.te_reached
            when(
              timing.vcc_offset_reached && timing.te_reached && (command =/= CtrlCommand.ColdReset || timing.th_reached)
            ) {
              state := CtrlState.Inactive
            }
          }

          is(CtrlCommand.Activate) {
            io.state.clock := True
            when(timing.clk_offset_reached) {
              state := CtrlState.Active
              command := CtrlCommand.Idle
            }
          }

          is(CtrlCommand.Reset, CtrlCommand.WarmReset) {
            io.iso.rst := False
            when(timing.te_reached) {
              state := CtrlState.Reset
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }
    }
  }
}

case class ISO7816Master() extends Component {
  val io = new Bundle {
    val iso = master(ISO7816())
    val config = in(ISO7816MasterConfig())
    val state = new Bundle {
      val tx_active = out Bool
      val rx_active = out Bool
    }
    val start = new Bundle {
      val tx = in Bool
      val rx = in Bool
    }
    val tx = slave(Stream(Bits(8 bit)))
    val rx = master(Flow(Bits(8 bit)))
  }

  io.iso.clk := False
  io.iso.rst := False
  io.iso.vcc := False

  io.rx.valid := False
  io.state.rx_active := False

  io.tx.ready := False
  io.iso.io.writeEnable := False
  io.iso.io.write := False

  val rx_error_bit_strb = False
  val timing = new Area {
    val en = False
    val resetCgt = False

    val etu = False
    val rx_sample = False

    val cnt = Reg(UInt(5 bit))
    when(cnt === 0 || !en) {
      cnt := 9
      etu := en
      rx_error_bit_strb := en
    } elsewhen (cnt === 4) {
      rx_sample := True
      cnt := cnt - 1
    } otherwise {
      cnt := cnt - 1
    }

    val cgtCnt = Reg(UInt(8 bits))
    val cgtOver = cgtCnt === 0
    when(resetCgt) {
      cgtCnt := io.config.cgt - 1
    } elsewhen (etu && !cgtOver) {
      cgtCnt := cgtCnt - 1
    }
  }

  io.state.tx_active := False
  val fsm = new StateMachine {
    val data = Reg(Bits(10 bits))
    val parity = Reg(Bool)

    val Idle = new State with EntryPoint

    val Tx = new State
    val TxParity = new State
    val TxWaitError = new State
    val TxErrorStop = new State
    val TxStop = new State

    val RxWait = new State
    val RxStart = new State
    val Rx = new State
    val RxParity = new State
    val RxStop = new State
    val RxError = new State
    val RxErrorPost = new State

    Idle.whenIsActive {
      timing.en := False
      when(io.start.tx && io.tx.valid) {
        goto(Tx)
      } elsewhen io.start.rx {
        goto(RxWait)
      }
    }

    // moment 0 - 9
    Tx.onEntry(
        {
          data := True ## io.tx.payload ## False // marker, data & startbit
          parity := False
          timing.resetCgt := True
        }
      )
      .whenIsActive {
        timing.en := True
        io.state.tx_active := True
        io.iso.io.write := data(0)
        io.iso.io.writeEnable := True
        when(timing.etu) {
          parity := parity ^ data(0)
          when(data(1 to 9) === B"1".resized) {
            goto(TxParity)
          } otherwise {
            data := data |>> 1
          }
        }
      }

    // moment 9 - 10
    TxParity.whenIsActive {
      timing.en := True
      io.state.tx_active := True
      io.iso.io.write := parity
      io.iso.io.writeEnable := True
      when(timing.etu) {
        goto(TxWaitError)
      }
    }

    // moment 10 - 11
    TxWaitError.whenIsActive {
      timing.en := True
      io.state.tx_active := False
      when(rx_error_bit_strb) {
        when(io.iso.io.read || !io.config.characterRepetition) {
          io.tx.ready := True
          goto(TxStop)
        } otherwise {
          goto(TxErrorStop)
        }
      }
    }

    // moment 12
    TxErrorStop.whenIsActive {
      timing.en := True
      io.state.tx_active := True
      when(timing.etu) {
        goto(TxStop)
      }
    }

    // moment 12 (no error) / 13 (error)
    TxStop.whenIsActive {
      timing.en := True
      io.state.tx_active := True
      when(timing.etu && timing.cgtOver) {
        when(io.tx.valid) {
          goto(Tx)
        } otherwise {
          goto(Idle)
        }
      }
    }

    io.rx.payload := data(1 to 8)
    RxWait.whenIsActive {
      when(io.iso.io.read === False) {
        goto(RxStart)
      }
    }

    RxStart.whenIsActive {
      timing.en := True
      data := B"1000000000"
      parity := True
      when(timing.etu) {
        goto(Rx)
      }
    }

    Rx.whenIsActive {
      timing.en := True
      io.state.rx_active := True
      when(timing.rx_sample) {
        data := io.iso.io.read ## data(1 to 9)
        parity := parity ^ io.iso.io.read
        when(data(1)) {
          goto(RxParity)
        }
      }
    }

    RxParity.whenIsActive {
      timing.en := True
      io.state.rx_active := True
      when(timing.rx_sample) {
        when(parity || !io.config.characterRepetition) {
          io.rx.valid := True
          goto(RxStop)
        } otherwise {
          goto(RxError)
        }
      }
    }

    RxStop.whenIsActive {
      timing.en := True
      when(timing.rx_sample) {
        goto(RxWait)
      }
    }

    RxError.whenIsActive {
      timing.en := True
      io.state.rx_active := True
      io.iso.io.write := False
      io.iso.io.writeEnable := True
      when(timing.rx_sample) {
        goto(RxErrorPost)
      }
    }

    RxErrorPost.whenIsActive {
      io.iso.io.write := True
      io.iso.io.writeEnable := True
      goto(RxWait)
    }
  }
}
