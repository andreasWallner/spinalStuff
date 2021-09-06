/* 

activate
cold reset

VCC  ------------_______________---------------------------------
CLK  --_____________________________-----------------------------
RST  --_______________________________________________-----------
I/O  -----____________________________________-------------------

     | te |                     |clk|
     |    vcc   |      th       |      ta    |
                                |          tb         |

warm reset

VCC  -------------------------------------
CLK  -------------------------------------
RST  ----___________________________------
IO   -----------___________---------------
        |  te  |
        |      ta         |
        |            tb             |

deactivate

VCC  -----------------------___________
CLK  __________________________________
RST  --------__________________________
IO   ---------------___________________
             | te  |
             |      vcc     |

*/

package andreasWallner.io.iso7816

import andreasWallner.spinaltap.ISpinalTAPCommModule
import spinal.core.ClockDomain.FixedFrequency
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axilite.{
  AxiLite4,
  AxiLite4Config,
  AxiLite4SlaveFactory
}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.fsm._
import spinal.lib.io.{TriState, TriStateArray}
import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.datamodel.BusComponent
import andreasWallner.registers.casemodel.Value

import scala.language.postfixOps

case class ISO7816() extends Bundle with IMasterSlave {
  val io = TriState(Bool())
  val vcc = Bool()
  val rst = Bool()
  val clk = Bool()

  def asMaster() = {
    master(io)
    out(vcc, rst, clk)
  }
}

case class CoreGenerics(
    dataDividerWidth: Int = 32,
    clockDividerWidth: Int = 32
) {}
case class PeripheralGenerics(
    rxBufferSize: Int = 256,
    txBufferSize: Int = 256,
    core: CoreGenerics = CoreGenerics()
) {}

case class ControlConfig() extends Bundle {
  val ta = UInt(32 bit)
  val tb = UInt(32 bit)
  val te = UInt(32 bit)
  val th = UInt(32 bit)
  val clk_offset = UInt(32 bit)
  val vcc_offset = UInt(32 bit)
}

case class RxTxConfig() extends Bundle {
  val characterRepetition = Bool()
  val cgt = UInt(8 bit) // in ETUs - 1 // TODO: size correctly,
}

// TODO: seperate timeouts for receive and reset?
// TODO: drive I/O transitions during activate/reset/... hard
// TODO: clean transitions to/from clockstop state
// TODO: only use th as criterium for next state?

object CtrlState extends SpinalEnum {
  val Inactive, Active, Reset, ClockStop = newElement()
}

object CtrlCommand extends SpinalEnum {
  val Idle, Deactivate, Activate, Reset, StopClock, ColdReset, WarmReset =
    newElement()
}

case class StateCtrl() extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val vcc = out Bool ()
      val rst = out Bool ()
      val io = master(TriState(Bool()))
    }
    val config = in(ControlConfig())
    val command = slave(Flow(CtrlCommand()))
    val start_rx = out Bool ()
    val tx_done = in Bool () // strobed on TX for clkstop min time
    val inhibit_tx = out Bool ()
    val state = new Bundle {
      val driving_io = out Bool ()
      val clock = out Bool ()
      val busy = out Bool ()
    }
  }

  val state = Reg(CtrlState()).init(CtrlState.Inactive)
  val command = Reg(CtrlCommand()).init(CtrlCommand.Idle)
  val active = command =/= CtrlCommand.Idle
  io.state.busy := active
  val timing = new Area {
    val reset = False
    val count = Reg(UInt(32 bit))
    when(reset) {
      count := 0
    } elsewhen active {
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
      io.iso.io.write := False
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
              timing.reset := True
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
              timing.reset := True
            }
          }

          is(CtrlCommand.StopClock) {
            when(!io.tx_done) { // TODO time
              state := CtrlState.ClockStop
              timing.reset := True
            }
          }

          is(CtrlCommand.Reset, CtrlCommand.WarmReset) {
            io.state.driving_io := True
            io.iso.io.writeEnable := False
            when(timing.te_reached) {
              state := CtrlState.Reset
              timing.reset := True
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }

      is(CtrlState.Reset) {
        switch(command) {
          is(CtrlCommand.Activate, CtrlCommand.WarmReset) {
            io.iso.rst := timing.tb_reached
            io.iso.io.writeEnable := !timing.ta_reached
            io.state.clock := timing.clk_offset_reached
            when(timing.ta_reached && timing.tb_reached) {
              state := CtrlState.Active
              command := CtrlCommand.Idle
              timing.reset := True
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
            io.iso.rst := False
            io.iso.io.write := False
            io.iso.io.writeEnable := timing.te_reached
            when(
              timing.vcc_offset_reached && timing.te_reached && (command =/= CtrlCommand.ColdReset || timing.th_reached)
            ) {
              state := CtrlState.Inactive
              timing.reset := True
            }
          }

          is(CtrlCommand.Activate) {
            io.state.clock := True
            when(timing.clk_offset_reached) {
              state := CtrlState.Active
              command := CtrlCommand.Idle
              timing.reset := True
            }
          }

          is(CtrlCommand.Reset, CtrlCommand.WarmReset) {
            io.iso.rst := False
            io.iso.io.writeEnable := timing.te_reached
            when(
              timing.te_reached && (command =/= CtrlCommand.WarmReset || timing.th_reached)
            ) {
              state := CtrlState.Reset
              timing.reset := True
            }
          }

          default { command := CtrlCommand.Idle }
        }
      }
    }
  }
}

case class ClockGen(divider_width: Int = 32) extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val clk = out Bool ()
    }
    val stop_clock = in Bool ()
    val vcc = in Bool ()
    val divider = in UInt (divider_width bit)
  }
  val clk = Reg(Bool())
  io.iso.clk := clk && io.vcc

  val cnt = Reg(UInt(divider_width bit))
  when(io.stop_clock || !io.vcc) {
    when(!io.vcc) {
      clk := False
    }
    cnt := 0
  } otherwise {
    when(cnt === 0) {
      cnt := io.divider
      clk := !clk
    } otherwise {
      cnt := cnt - 1
    }
  }
}

case class Timeout() extends Component {
  val io = new Bundle {
    val cwt = in UInt()
    val bwt = in UInt()

    val en = in Bool()
    val activity = in Bool()
    val c_timeout = out Bool() setAsReg
    val b_timeout = out Bool() setAsReg
  }

  val cnt = Reg(UInt(32 bit))
  val first = Reg(Bool())
  when(!io.en || io.activity) {
    cnt := 0
  } otherwise {
    cnt := cnt + 1
  }

  first.setWhen(!io.en)
  first.clearWhen(io.activity)
  io.c_timeout.setWhen(cnt === io.cwt)
  io.c_timeout.clearWhen(!io.en)
  io.b_timeout.setWhen(cnt === io.bwt && first)
  io.b_timeout.clearWhen(!io.en)
}

case class RxTxCore() extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val io = master(TriState(Bool()))
    }
    val config = in(RxTxConfig())
    val state = new Bundle {
      val tx_active = out Bool ()
      val rx_active = out Bool ()
      val activity = out Bool()
    }
    val trigger = new Bundle {
      val tx = in Bool ()
      val rx = in Bool ()
      val cancel = in Bool()
    }
    val tx = slave(Stream(Bits(8 bit)))
    val rx = master(Flow(Bits(8 bit)))
  }

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
  io.state.activity := False
  val fsm = new StateMachine {
    val data = Reg(Bits(10 bits))
    val parity = Reg(Bool())

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

    always {
      when(io.trigger.cancel) {
        goto(Idle)
      }
    }

    Idle.whenIsActive {
      timing.en := False
      when(io.trigger.tx && io.tx.valid) {
        goto(Tx)
      } elsewhen io.trigger.rx {
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
      io.state.rx_active := True
      when(io.iso.io.read === False) {
        io.state.activity := True
        goto(RxStart)
      }
    }

    RxStart.whenIsActive {
      io.state.rx_active := True
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
      io.state.rx_active := True
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
      io.state.rx_active := True
      io.iso.io.write := True
      io.iso.io.writeEnable := True
      goto(RxWait)
    }
  }
}

case class ISO7816Master(generics: CoreGenerics) extends Component {
  val io = new Bundle {
    val iso = master(ISO7816())
    val tx = slave(Stream(Bits(8 bit)))
    val rx = master(Flow(Bits(8 bit)))
    val config = new Bundle {
      val control = in(ControlConfig())
      val rxtx = in(RxTxConfig())
      val clockrate = in(UInt(32 bit))
      val bwt = in UInt(32 bit)
      val cwt = in UInt(32 bit)
    }
    val state = new Bundle {
      val tx_active = out Bool ()
      val rx_active = out Bool ()
      val change_active = out Bool ()
    }
    val start = new Bundle {
      val tx = in Bool ()
      val rx = in Bool ()
      val deactivate = in Bool ()
      val activate = in Bool ()
      val reset = in Bool ()
      val stop_clock = in Bool ()
    }
  }

  val rxtx = RxTxCore()
  val control = StateCtrl()
  val clock = ClockGen()
  val timeout = Timeout()

  val start_non_rx = io.start.tx || io.start.deactivate || io.start.activate || io.start.reset || io.start.stop_clock
  val auto_rx_primed = Reg(Bool()) init False
  auto_rx_primed.setWhen(io.start.rx && !rxtx.io.trigger.rx)
  auto_rx_primed.clearWhen(rxtx.io.state.rx_active || rxtx.io.trigger.cancel)

  io.iso.vcc := control.io.iso.vcc
  io.iso.rst := control.io.iso.rst
  io.iso.clk := clock.io.iso.clk
  rxtx.io.iso.io.read := io.iso.io.read
  control.io.iso.io.read := io.iso.io.read
  io.iso.io.write := control.io.state.driving_io ? control.io.iso.io.write | rxtx.io.iso.io.write
  io.iso.io.writeEnable := control.io.state.driving_io ? control.io.iso.io.writeEnable | rxtx.io.iso.io.writeEnable

  rxtx.io.config := io.config.rxtx
  rxtx.io.rx <> io.rx
  rxtx.io.tx <> io.tx
  rxtx.io.trigger.rx := (!start_non_rx && io.start.rx) || (auto_rx_primed && (control.io.state.busy.fall() || rxtx.io.state.tx_active.fall()))
  rxtx.io.trigger.tx := io.start.tx
  io.state.tx_active := rxtx.io.state.tx_active
  io.state.rx_active := rxtx.io.state.rx_active || auto_rx_primed

  timeout.io.bwt := io.config.bwt
  timeout.io.cwt := io.config.cwt
  timeout.io.en := rxtx.io.state.rx_active
  timeout.io.activity := rxtx.io.state.activity
  rxtx.io.trigger.cancel := timeout.io.c_timeout || timeout.io.b_timeout

  control.io.config := io.config.control
  control.io.command.default(CtrlCommand.Idle)
  when(io.start.activate && io.start.deactivate) {
    control.io.command.push(CtrlCommand.ColdReset)
  } elsewhen (io.start.reset && io.start.activate) {
    control.io.command.push(CtrlCommand.WarmReset)
  } elsewhen (io.start.deactivate) {
    control.io.command.push(CtrlCommand.Deactivate)
  } elsewhen (io.start.activate) {
    control.io.command.push(CtrlCommand.Activate)
  } elsewhen (io.start.reset) {
    control.io.command.push(CtrlCommand.Reset)
  } elsewhen (io.start.stop_clock) {
    control.io.command.push(CtrlCommand.StopClock)
  }
  control.io.tx_done := False // TODO
  io.state.change_active := control.io.state.busy

  clock.io.stop_clock := !control.io.state.clock
  clock.io.vcc := control.io.iso.vcc
  clock.io.divider := io.config.clockrate
}

class Peripheral[T <: spinal.core.Data with IMasterSlave](
    generic: PeripheralGenerics,
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component with BusComponent {
  val io = new Bundle {
    val iso = master(ISO7816())
    val bus = slave(busType())
  }

  val rxFifo = StreamFifo(Bits(8 bit), generic.rxBufferSize)
  val txFifo = StreamFifo(Bits(8 bit), generic.txBufferSize)
  val core = ISO7816Master(generic.core)
  io.iso <> core.io.iso
  txFifo.io.pop >> core.io.tx
  val rxFifoIsOverflow = Bool()
  core.io.rx.toStream(rxFifoIsOverflow) >> rxFifo.io.push

  val factory = new BusSlaveFactoryRecorder(metaFactory(io.bus))

  val frequency = clockDomain.frequency match {
    case FixedFrequency(value) => value.toLong
    case _                     => 0L
  }
  factory.register(0x0, "info0").read(U(frequency, 32 bit), 0, "frequency", "Used module frequency")

  val info1 = factory.register(0x04, "info1")
  val rxBufferSize = UInt(16 bits)
  rxBufferSize := generic.rxBufferSize
  val txBufferSize = UInt(16 bits)
  txBufferSize := generic.txBufferSize
  info1.read(rxBufferSize, 0, "rx_buffer_size")
  info1.read(txBufferSize, 16, "tx_buffer_size")

  val state = factory.register(0x08, "state")
  state.read(core.io.state.rx_active, 0, "rx_active")
  state.read(core.io.state.tx_active, 1, "tx_active")
  state.read(core.io.state.change_active, 2, "change_active")
  state.doBitsAccumulationAndClearOnRead(rxFifoIsOverflow.asBits, 3, "rx_ovfl")
  state.doBitsAccumulationAndClearOnRead(txFifo.io.push.isStall.asBits, 4, "tx_ovfl")

  val config = factory.register(0x0c, "config")
  core.io.config.rxtx.characterRepetition := config.createReadAndWrite(
    Bool(),
    0,
    "charrep",
    "Character repetition setting",
    List(
      Value(0, "dis", "disabled"),
      Value(1, "en", "enabled")
    )
  )
  core.io.config.rxtx.cgt := config.createReadAndWrite(
    UInt(core.io.config.rxtx.cgt.getWidth bits),
    1,
    "cgt",
    "character guard time in module clocks"
  )

  val trigger = factory.register(0x10, "trigger")
  core.io.start.rx := False
  core.io.start.tx := False
  core.io.start.reset := False
  core.io.start.deactivate := False
  core.io.start.activate := False
  core.io.start.stop_clock := False
  factory.f.setOnSet(core.io.start.rx, 0x10, 0)
  //trigger.setOnSet(core.io.start.rx, 0, "rx")
  trigger.setOnSet(core.io.start.tx, 1, "tx")
  trigger.setOnSet(core.io.start.reset, 2, "reset")
  trigger.setOnSet(core.io.start.deactivate, 3, "deactivate")
  trigger.setOnSet(core.io.start.activate, 4, "activate")
  trigger.setOnSet(core.io.start.stop_clock, 5, "stop_clock")

  val config0 = factory.register(0x14, "config0")
  val defaultClkDivider = frequency / 3200000
  core.io.config.clockrate := config0.createReadAndWrite(UInt(32 bit), 0, "divider")
  // divider

  core.io.config.control.ta := factory
    .register(0x18, "config1")
    .createReadAndWrite(UInt(32 bits), 0, "ta")
    .init(500 * defaultClkDivider)
  core.io.config.control.tb := factory
    .register(0x1c, "config2")
    .createReadAndWrite(UInt(32 bits), 0, "tb")
    .init(1000 * defaultClkDivider)
  core.io.config.control.te := factory
    .register(0x20, "config3")
    .createReadAndWrite(UInt(32 bits), 0, "te")
    .init(0)
  core.io.config.control.th := factory
    .register(0x24, "config4")
    .createReadAndWrite(UInt(32 bits), 0, "th")
    .init(frequency / 100000000)
  core.io.config.control.vcc_offset := factory
    .register(0x28, "config5")
    .createReadAndWrite(UInt(32 bits), 0, "vcc_offset")
    .init(0)
  core.io.config.control.clk_offset := factory
    .register(0x2c, "config6")
    .createReadAndWrite(UInt(32 bits), 0, "clk_offset")
    .init(0)

  val buffers = factory.register(0x30, "buffers")
  buffers.read(rxFifo.io.occupancy.resize(16 bit), 0, "rx_occupancy")
  buffers.read(txFifo.io.availability.resize(16 bits), 16, "tx_available")

  val txFifoOverflow = Bool()
  factory
    .register(0x3c, "rx")
    .readStreamNonBlocking(rxFifo.io.pop, 31, 0, "data")
  factory
    .register(0x40, "tx")
    .createAndDriveFlow(Bits(txFifo.io.push.payload.getWidth bits), 0, "data")
    .toStream(txFifoOverflow) <> txFifo.io.push

  core.io.config.bwt := factory.register(0x44, "config7").createReadAndWrite(UInt(32 bit), 0, "bwt")
  core.io.config.cwt := factory.register(0x48, "config8").createReadAndWrite(UInt(32 bit), 0, "cwt")

  override def elements = factory.elements
  override def busComponentName = "ISO7816"
  override def dataWidth = factory.dataWidth
  override def wordAddressInc = factory.wordAddressInc
}

case class Apb3ISO7816Peripheral(
    generics: PeripheralGenerics = PeripheralGenerics(),
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends Peripheral[Apb3](
      generics,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    )

case class AxiLite4ISO7816Peripheral(
    generics: PeripheralGenerics = PeripheralGenerics(),
    busConfig: AxiLite4Config = AxiLite4Config(12, 32)
) extends Peripheral[AxiLite4](
      generics,
      AxiLite4(busConfig),
      new AxiLite4SlaveFactory(_)
    )
