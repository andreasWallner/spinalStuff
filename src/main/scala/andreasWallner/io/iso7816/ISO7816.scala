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
    dataPrescalerWidth: Int = 32,
    clockPrescalerWidth: Int = 32
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

case class ClockGen(prescaler_width: Int = 32) extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val clk = out Bool ()
    }
    val stop_clock = in Bool ()
    val vcc = in Bool ()
    val divider = in UInt (prescaler_width bit)
  }
  val clk = Reg(Bool())
  io.iso.clk := clk && io.vcc

  val cnt = Reg(UInt(prescaler_width bit))
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

case class RxTxCore() extends Component {
  val io = new Bundle {
    val iso = new Bundle {
      val io = master(TriState(Bool()))
    }
    val config = in(RxTxConfig())
    val state = new Bundle {
      val tx_active = out Bool ()
      val rx_active = out Bool ()
    }
    val start = new Bundle {
      val tx = in Bool ()
      val rx = in Bool ()
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

case class ISO7816Master(generics: CoreGenerics) extends Component {
  val io = new Bundle {
    val iso = master(ISO7816())
    val tx = slave(Stream(Bits(8 bit)))
    val rx = master(Flow(Bits(8 bit)))
    val config = new Bundle {
      val control = in(ControlConfig())
      val rxtx = in(RxTxConfig())
      val clockrate = in(UInt(32 bit))
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

  val start_non_rx = io.start.tx || io.start.deactivate
  val last_rx = RegNextWhen(io.start.rx, start_non_rx, False)

  io.iso.vcc := control.io.iso.vcc
  io.iso.rst := control.io.iso.rst
  io.iso.clk := clock.io.iso.clk
  rxtx.io.iso.io.read := io.iso.io.read
  control.io.iso.io.read := io.iso.io.read
  io.iso.io.write := control.io.state.driving_io ? rxtx.io.iso.io.write | control.io.iso.io.write
  io.iso.io.writeEnable := control.io.state.driving_io ? control.io.iso.io.writeEnable | rxtx.io.iso.io.writeEnable

  rxtx.io.config := io.config.rxtx
  rxtx.io.rx <> io.rx
  rxtx.io.tx <> io.tx
  rxtx.io.start.rx := io.start.rx
  rxtx.io.start.tx := io.start.tx
  io.state.tx_active := rxtx.io.state.tx_active
  io.state.rx_active := (!start_non_rx && rxtx.io.state.rx_active) || (last_rx && control.io.start_rx)

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

  clock.io.stop_clock := control.io.state.clock
  clock.io.vcc := control.io.iso.vcc
  clock.io.divider := io.config.clockrate

  // TODO: timeouts
}

abstract class Peripheral[T <: spinal.core.Data with IMasterSlave](
    generic: PeripheralGenerics,
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component
    with ISpinalTAPCommModule[T] {
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

  val factory = metaFactory(io.bus)

  val frequency = clockDomain.frequency match {
    case FixedFrequency(value) => value.toLong
    case _                     => 0L
  }
  factory.read(U(frequency), 0, 0)

  val rxBufferSize = UInt(16 bits)
  rxBufferSize := generic.rxBufferSize
  val txBufferSize = UInt(16 bits)
  txBufferSize := generic.txBufferSize
  factory.read(rxBufferSize, 4, 0)
  factory.read(txBufferSize, 4, 16)

  factory.read(core.io.state.rx_active, 8, 0)
  factory.read(core.io.state.tx_active, 8, 1)
  factory.read(core.io.state.change_active, 8, 2)
  factory.doBitsAccumulationAndClearOnRead(rxFifoIsOverflow.asBits, 8, 3)
  factory.doBitsAccumulationAndClearOnRead(txFifo.io.push.isStall.asBits, 8, 4)

  core.io.config.rxtx.characterRepetition := factory.createReadAndWrite(
    Bool(),
    12,
    0
  )
  core.io.config.rxtx.cgt := factory.createReadAndWrite(
    UInt(core.io.config.rxtx.cgt.getWidth bits),
    12,
    1
  )

  core.io.start.rx := False
  core.io.start.tx := False
  core.io.start.reset := False
  core.io.start.deactivate := False
  core.io.start.activate := False
  core.io.start.stop_clock := False
  factory.setOnSet(core.io.start.rx, 16, 0)
  factory.setOnSet(core.io.start.tx, 16, 1)
  factory.setOnSet(core.io.start.reset, 16, 2)
  factory.setOnSet(core.io.start.deactivate, 16, 3)
  factory.setOnSet(core.io.start.activate, 16, 4)
  factory.setOnSet(core.io.start.stop_clock, 16, 5)

  val defaultClkDivider = frequency / 3200000
  core.io.config.clockrate := factory.createReadAndWrite(UInt(32 bit), 20)
  // prescaler

  core.io.config.control.ta := factory
    .createReadAndWrite(UInt(32 bits), 28)
    .init(500 * defaultClkDivider)
  core.io.config.control.tb := factory
    .createReadAndWrite(UInt(32 bits), 32)
    .init(1000 * defaultClkDivider)
  core.io.config.control.te := factory
    .createReadAndWrite(UInt(32 bits), 36)
    .init(0)
  core.io.config.control.th := factory
    .createReadAndWrite(UInt(32 bits), 40)
    .init(frequency / 100000000)
  core.io.config.control.vcc_offset := factory
    .createReadAndWrite(UInt(32 bits), 44)
    .init(0)
  core.io.config.control.clk_offset := factory
    .createReadAndWrite(UInt(32 bits), 48)
    .init(0)

  factory.read(rxFifo.io.occupancy.resize(16 bit), 52, 0)
  factory.read(txFifo.io.availability.resize(16 bits), 52, 16)

  val txFifoOverflow = Bool()
  factory.readStreamNonBlocking(rxFifo.io.pop, 56)
  factory
    .createAndDriveFlow(Bits(txFifo.io.push.payload.getWidth bits), 60)
    .toStream(txFifoOverflow) <> txFifo.io.push

  override def bus() = io.bus
  override def triggerOutputs() = List()
  override def triggerInputs() = List()
  override def ports() = {
    val tri = TriStateArray(5 bit)
    tri(0) <> io.iso.io

    val rstTri = tri(1)
    rstTri.write := io.iso.rst
    rstTri.writeEnable := True

    val clkTri = tri(2)
    clkTri.write := io.iso.clk
    clkTri.writeEnable := True

    for (i <- 3 until 5) {
      val rfuTri = tri(i)
      rfuTri.write := False
      rfuTri.writeEnable := False
    }

    tri
  }
  override def vcc() = io.iso.vcc
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
