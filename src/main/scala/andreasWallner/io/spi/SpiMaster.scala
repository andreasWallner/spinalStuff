package andreasWallner.io.spi

import andreasWallner.registers.{BusSlaveFactoryRecorder}
import andreasWallner.registers.casemodel.Value
import andreasWallner.registers.datamodel.BusComponent
import spinal.core.ClockDomain.FixedFrequency
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.language.postfixOps

// TODO allow for low dividers by accounting for delay from synchronizing, etc.

case class SpiType() extends Bundle {
  val cpol = Bool
  val cpha = Bool
}

case class Spi() extends Bundle with IMasterSlave {
  val sclk = Bool()
  val mosi = Bool()
  val miso = Bool()
  val cs = Bool()

  override def asMaster(): Unit = {
    out(sclk, mosi, cs)
    in(miso)
  }
}

object SpiMaster {
  case class CoreParameter(
      dividerWidth: Int = 28,
      datawidth: Int = 8,
      wordGuardClocksWidth: Int = 4,
      csAssertGuardWidth: Int = 4,
      csDeassertGuardWidth: Int = 4
  ) {}

  case class PeripheralParameter(
      rxBufferSize: Int = 256,
      txBufferSize: Int = 256,
      core: CoreParameter = CoreParameter()
  ) {
    def rxBufferSizeBits = log2Up(rxBufferSize) bits
    def txBufferSizeBits = log2Up(txBufferSize) bits
  }

  case class CoreConfig(p: CoreParameter) extends Bundle {
    val divider = UInt(p.dividerWidth bits)
    val spiType = SpiType()
    val msbFirst = Bool()
    val csActiveState = Bool()
    val wordGuardClocks = UInt(p.wordGuardClocksWidth bits)
    val csAssertGuard = UInt(p.csAssertGuardWidth bits)
    val csDeassertGuard = UInt(p.csDeassertGuardWidth bits)
  }

  case class Core(p: CoreParameter = CoreParameter()) extends Component {
    val io = new Bundle {
      val spi = master(Spi())
      val config = in(CoreConfig(p))
      val busy = out Bool ()
      val txData = slave(Stream(Fragment(Bits(p.datawidth bit))))
      val rxData = master(Flow(Bits(p.datawidth bits)))
      val trigger = new Bundle {
        val assert = in Bool ()
        val transfer = in Bool ()
        val deassert = in Bool ()
      }
    }

    val sync = new Area {
      val miso = BufferCC(io.spi.miso)
    }
    val csActive = Reg(Bool()) init False
    val clkActive = Reg(Bool()) init False
    io.spi.sclk := RegNext(clkActive ^ io.config.spiType.cpol)
    io.spi.cs := RegNext(csActive ^ !io.config.csActiveState)

    val runTiming = Bool()
    val transferring = Bool()
    val lastWord = Reg(Bool())
    val lastPhase = Bool()

    val timing = new Area {
      val counter = Reg(UInt(p.dividerWidth bits))
      val phase = Reg(Bool())
      val guardCnt = Reg(UInt(p.wordGuardClocksWidth bit))

      when(!runTiming || counter === 0) {
        counter := io.config.divider
      } otherwise {
        counter := counter - 1
      }

      val strobe = False
      val isSample = !phase ^ io.config.spiType.cpha

      when(!runTiming) {
        guardCnt := io.config.wordGuardClocks
        phase := False
      } otherwise {
        when(counter === 0) {
          when(lastPhase && !isSample && (guardCnt =/= 0) && !lastWord) {
            guardCnt := guardCnt - 1
          } otherwise {
            guardCnt := io.config.wordGuardClocks
            strobe := True
          }
        }
      }

      when(transferring && strobe) {
        phase := !phase
      }

      val sample = isSample && strobe
      val update = (!isSample && strobe) || (transferring
        .rise() && !io.config.spiType.cpha)
      val lastStrobe = lastPhase && lastWord && ((sample && io.config.spiType.cpha) || (update && !io.config.spiType.cpha))
    }

    val txWord = Reg(Bits(p.datawidth + 1 bits))
    lastPhase := (txWord >> 1) === B"1".resized

    transferring := False
    runTiming := False
    val sm = new StateMachine {
      val anyTrigger = io.trigger.asBits.orR
      val didTriggerTransfer = Reg(Bool())
      val didTriggerDeassert = Reg(Bool())

      val Idle = new State with EntryPoint
      val Assert = new State
      val Transfer = new State
      val Deassert = new State

      Idle.whenIsActive {
        when(anyTrigger) {
          didTriggerTransfer := io.trigger.transfer
          didTriggerDeassert := io.trigger.deassert
          txWord := B"10".resized
          lastWord := io.txData.payload.last
          when(io.trigger.assert) {
            goto(Assert)
          } elsewhen (io.trigger.transfer) {
            goto(Transfer)
          } elsewhen (io.trigger.deassert) {
            goto(Deassert)
          }
        }
      }

      Assert
        .onEntry {
          timing.guardCnt := io.config.csAssertGuard
          csActive := True
        }
        .whenIsActive {
          runTiming := True
          when(timing.guardCnt === 0) {
            when(didTriggerTransfer) {
              goto(Transfer)
            } elsewhen (didTriggerDeassert) {
              goto(Deassert)
            }
          }
        }

      Transfer
        .whenIsActive {
          runTiming := True
          transferring := True
          when(timing.lastStrobe) {
            when(didTriggerDeassert) {
              goto(Deassert)
            } otherwise {
              goto(Idle)
            }
          }
        }

      Deassert
        .onEntry {
          timing.guardCnt := io.config.csDeassertGuard
        }
        .whenIsActive {
          runTiming := True
          when(timing.guardCnt === 0) { goto(Idle) }
        }
        .onExit {
          csActive := True
        }
    }
    io.busy := !(sm.isActive(sm.Idle) || sm.isActive(sm.stateBoot))
    io.txData.ready := False

    val fsm = new Area {
      val rxWord = Reg(Bits(p.datawidth bits))
      val rxReadyNext = False

      when(transferring) {
        when(timing.strobe) {
          clkActive := !clkActive
        }
        when(timing.update) {
          when(lastPhase) {
            txWord := B"1" ## Mux(
              io.config.msbFirst,
              io.txData.payload.fragment.reversed,
              io.txData.payload.fragment
            )
            lastWord := io.txData.payload.last
            io.txData.ready := True
          } otherwise {
            txWord := txWord |>> 1
          }
        }
        when(timing.sample) {
          rxWord := sync.miso ## (rxWord >> 1)
          when(lastPhase) {
            rxReadyNext := True
          }
        }
      }
    }
    io.rxData.valid := RegNext(fsm.rxReadyNext) init False
    io.spi.mosi := txWord(0)
    io.rxData.payload := Mux(
      io.config.msbFirst,
      fsm.rxWord.reversed,
      fsm.rxWord
    )
  }

  class Ctrl[T <: spinal.core.Data with IMasterSlave](
      p: PeripheralParameter,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends Component
      with BusComponent {
    val io = new Bundle {
      val bus = slave(busType())
      val spi = master(Spi())
    }

    val core = Core(p.core)
    core.io.spi <> io.spi

    val factory = metaFactory(io.bus)
    val f = new BusSlaveFactoryRecorder(factory)

    val rxFifo =
      StreamFifo(Bits(p.rxBufferSizeBits), p.rxBufferSize)
    val txFifo =
      StreamFifo(Bits(p.txBufferSizeBits), p.txBufferSize)

    // info registers
    val info0 = f.register(0x0, "info0")
    info0.read(U(0), 0, "version")

    val info1 = f.register(0x4, "info1")
    val frequency = clockDomain.frequency match {
      case FixedFrequency(value) => value.toLong
      case _                     => 0L
    }
    info1.read(
      U(frequency, 32 bit),
      0,
      "frequency",
      "Frequency the module is supplied with"
    )

    val info2 = f.register(0x08, "info2")
    info2.read(U(p.rxBufferSize, 16 bit), 0, "rx_buf_size", "Size of RX buffer")
    info2.read(
      U(p.txBufferSize, 16 bit),
      16,
      "tx_buf_size",
      "Size of TX buffer"
    )

    val info3 = f.register(0x0c, "info3")
    info3.read(
      U(p.core.dividerWidth),
      0,
      "divider_width",
      "Width of frequency divider"
    )

    // status registers
    val status = f.register(0x10, "status")
    status.read(
      core.io.busy,
      0,
      "busy",
      "Shows if the module is currently working on a trigger",
      List(
        Value(0, "idle", "module is idle"),
        Value(1, "busy", "module is busy")
      )
    )
    status.doBitsAccumulationAndClearOnRead( // tx overflow?
      rxFifo.io.push.isStall.asBits,
      1,
      "tx_ovfl",
      "Indicates that more values where pushed into the TX buffer than it can hold, cleared on read",
      List(
        Value(0, "ok", "no overflow happened"),
        Value(1, "ovfl", "overflow happened since last read")
      )
    )
    status.read(
      rxFifo.io.occupancy,
      12,
      "rx_occupancy",
      "Number of bytes currently stored in RX FIFO"
    )
    status.read(
      txFifo.io.occupancy,
      22,
      "tx_occupancy",
      "Number of bytes currently stored in TX FIFO"
    )

    // config register
    val config = f.register(0x18, "config", "Runtime peripheral configuration")
    core.io.config.spiType.cpha := config.createReadAndWrite(
      Bool,
      0,
      "cpha",
      "Used phase setting",
      List(
        Value(
          0,
          "before",
          "Shift first data bit on CS/before first clock edge"
        ),
        Value(
          1,
          "edge",
          "Shift first data bit with first edge, second edge samples"
        )
      )
    ) init False
    core.io.config.spiType.cpol := config.createReadAndWrite(
      Bool,
      1,
      "cpol",
      "Used clock polarity",
      List(
        Value(0, "low", "Idle clock level is low"),
        Value(1, "high", "Idle clock level is high")
      )
    ) init False
    core.io.config.msbFirst := config.createReadAndWrite(
      Bool,
      2,
      "bitorder",
      "Bitorder for RX/TX",
      List(
        Value(0, "lsbfirst", "LS-bit is sent first"),
        Value(1, "msbfirst", "MS-bit is sent first")
      )
    ) init False
    core.io.config.csActiveState := config.createReadAndWrite(
      Bool,
      3,
      "ss",
      "Active state for SS line",
      List(
        Value(0, "active_low", "SS line is active low"),
        Value(1, "active_high", "SS line is active high")
      )
    ) init False
    core.io.config.divider := config.createReadAndWrite(
      UInt(p.core.dividerWidth bits),
      4,
      "divider",
      "Divider configuring the used SPI clock speed (clk = fsys / divider)"
    ) init 100

    // trigger register
    val trigger = f.register(0x1c, "trigger")
    val flush = Bool()
    rxFifo.io.flush := flush
    txFifo.io.flush := flush

    core.io.trigger.assert := False
    core.io.trigger.transfer := False
    core.io.trigger.deassert := False
    flush := False
    trigger.setOnSet(
      core.io.trigger.assert,
      0,
      "assert",
      "Trigger assertion of SS",
      List(
        Value(0, "noop", "No action"),
        Value(1, "trigger", "Assert SS before other actions")
      )
    )
    trigger.setOnSet(
      core.io.trigger.transfer,
      1,
      "transceive",
      "Tigger transceive of data in TX buffer",
      List(
        Value(0, "noop", "No action"),
        Value(1, "trigger", "Transceive data after possible CS assertion")
      )
    )
    trigger.setOnSet(
      core.io.trigger.deassert,
      2,
      "deassert",
      "Trigger deassertion of SS",
      List(
        Value(0, "noop", "No action"),
        Value(1, "trigger", "Deassert SS after possible transceive of data")
      )
    )
    trigger.setOnSet(
      flush,
      31,
      "flush",
      "Flush TX/RX FIFOs",
      List(Value(0, "noop", "No action"), Value(1, "flush", "Flush FIFOs"))
    )

    // rx register
    val rx = f.register(0x20, "rx", "Gateway to RX FIFO")
    rxFifo.io.push <> core.io.rxData.toStream
    rx.readStreamNonBlocking(rxFifo.io.pop, 31, 0, "data", "Byte in RX buffer")

    // tx register
    val tx = f.register(0x24, "tx", "Gateway to TX FIFO")
    txFifo.io.push <> tx
      .createAndDriveFlow(Bits(p.core.datawidth bits), 0, "data", "Enqueues data in the TX FIFO")
      .toStream
    core.io.txData <> txFifo.io.pop.addFragmentLast(txFifo.io.occupancy === 1)

    // guard times
    val guard_times = f.register(0x28, "guard_times", "Guard times used during communication")
    core.io.config.wordGuardClocks := guard_times.createReadAndWrite(
      UInt(p.core.wordGuardClocksWidth bits),
      0, "word", "Number of clock durations of pause between individual sent bytes"
    ) init 0
    core.io.config.csAssertGuard := guard_times.createReadAndWrite(
      UInt(p.core.csAssertGuardWidth bits),
      8, "assert", "Number of clock durations pause between SS assertion and transceive start"
    ) init 1
    core.io.config.csDeassertGuard := guard_times.createReadAndWrite(
      UInt(p.core.csDeassertGuardWidth bits),
      16, "deassert", "Number of clock durations pause between transceive end and SS deassert"
    ) init 1

    override def elements = f.elements
    override def busComponentName = "SPI"
    override def dataWidth = f.dataWidth
    override def wordAddressInc = f.wordAddressInc
  }
}

case class Apb3SpiMaster(
    generics: SpiMaster.PeripheralParameter = SpiMaster.PeripheralParameter(),
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends SpiMaster.Ctrl[Apb3](
      generics,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    ) {}
