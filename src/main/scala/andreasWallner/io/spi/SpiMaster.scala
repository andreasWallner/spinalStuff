package andreasWallner.io.spi

import andreasWallner.registers.casemodel._
import andreasWallner.registers.datamodel.{BusComponent, AccessType}
import spinal.core.ClockDomain.FixedFrequency
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.collection.mutable
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
    val regs = mutable.MutableList[Register]()

    val core = Core(p.core)
    core.io.spi <> io.spi

    val factory = metaFactory(io.bus)
    val rxFifo =
      StreamFifo(Bits(p.rxBufferSizeBits), p.rxBufferSize)
    val txFifo =
      StreamFifo(Bits(p.txBufferSizeBits), p.txBufferSize)

    // info registers
    factory.read(U(0), 0x0, 0)

    val frequency = clockDomain.frequency match {
      case FixedFrequency(value) => value.toLong
      case _                     => 0L
    }
    factory.read(U(frequency, 32 bit), 0x04, 0)
    regs += Register(
      "info1",
      0x4,
      "Peripheral Information",
      List(
        Field(
          "frequency",
          UInt(32 bit),
          31 downto 0,
          AccessType.RO,
          0,
          false,
          "Frequency the module is supplied with",
          List(Value(0, "unknown"))
        )
      )
    )

    factory.read(U(p.rxBufferSize, 16 bit), 0x08, 0)
    factory.read(U(p.txBufferSize, 16 bit), 0x08, 16)
    regs += Register(
      "info2",
      0x8,
      "Peripheral Information",
      List(
        Field(
          "rx_buf_size",
          UInt(16 bit),
          15 downto 0,
          AccessType.RO,
          p.rxBufferSize,
          false,
          "Size of RX buffer"
        ),
        Field(
          "tx_buf_size",
          UInt(16 bit),
          31 downto 16,
          AccessType.RO,
          p.txBufferSize,
          false,
          "Size of TX buffer"
        )
      )
    )

    factory.read(U(p.core.dividerWidth), 0x0c, 0)
    regs += Register(
      "info3",
      0x0c,
      "Peripheral Information",
      List(
        Field(
          "divider_width",
          UInt(32 bit),
          31 downto 0,
          AccessType.RO,
          p.core.dividerWidth,
          false,
          "Width of divider"
        )
      )
    )

    // status registers
    factory.read(core.io.busy, 0x10, 0)
    factory.doBitsAccumulationAndClearOnRead( // tx overflow?
      rxFifo.io.push.isStall.asBits,
      0x10,
      1
    )
    factory.read(rxFifo.io.occupancy, 0x10, 12)
    factory.read(txFifo.io.occupancy, 0x10, 22)
    regs += Register(
      "status",
      0x10,
      "",
      List(
        Field(
          "busy",
          Bool(),
          0 downto 0,
          AccessType.RO,
          0,
          false,
          "Shows if the module is currently working on a trigger",
          List(
            Value(0, "idle", "module is idle"),
            Value(1, "busy", "module is busy")
          )
        ),
        Field(
          "tx_ovfl",
          Bool(),
          1 downto 1,
          AccessType.RC,
          0,
          false,
          "Indicates that more values where pushed into the TX buffer than it can hold, cleared on read",
          List(
            Value(0, "ok", "no overflow happened"),
            Value(1, "ovfl", "overflow happened since last read")
          )
        ),
        Field(
          "rx_occupancy",
          UInt(16 bit),
          21 downto 12,
          AccessType.RO,
          0,
          false,
          "Number of bytes currently stored in RX FIFO"
        ),
        Field(
          "tx_occupancy",
          UInt(16 bit),
          31 downto 22,
          AccessType.RO,
          0,
          false,
          "Number of bytes currently stored in RX FIFO"
        )
      )
    )

    // config register
    core.io.config.spiType.cpha := factory.createReadAndWrite(Bool, 0x18, 0) init False
    core.io.config.spiType.cpol := factory.createReadAndWrite(Bool, 0x18, 1) init False
    core.io.config.msbFirst := factory.createReadAndWrite(Bool, 0x18, 2) init False
    core.io.config.csActiveState := factory.createReadAndWrite(Bool, 0x18, 3) init False
    core.io.config.divider := factory.createReadAndWrite(
      UInt(p.core.dividerWidth bits),
      0x18,
      4
    )
    regs += Register(
      "config",
      0x18,
      "Runtime peripheral configuration",
      List(
        Field(
          "cpha",
          Bool(),
          0 downto 0,
          AccessType.RW,
          0,
          false,
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
        ),
        Field(
          "cpol",
          Bool(),
          1 downto 1,
          AccessType.RW,
          0,
          false,
          "Used clock polarity",
          List(
            Value(0, "low", "Idle clock level is low"),
            Value(1, "high", "Idle clock level is high")
          )
        ),
        Field(
          "bitorder",
          Bool(),
          2 downto 2,
          AccessType.RW,
          0,
          false,
          "Bitorder for RX/TX",
          List(
            Value(0, "lsbfirst", "LS-bit is sent first"),
            Value(1, "msbfirst", "MS-bit is sent first")
          )
        ),
        Field(
          "ss",
          Bool(),
          3 downto 3,
          AccessType.RW,
          0,
          false,
          "Active state for SS line",
          List(
            Value(0, "active_low", "SS line is active low"),
            Value(1, "active_high", "SS line is active high")
          )
        ),
        Field(
          "divider",
          UInt(p.core.dividerWidth bits),
          p.core.dividerWidth + 4 downto 4,
          AccessType.RW,
          0,
          false,
          "Divider configuring the used SPI clock speed (clk = fsys / divider)"
        )
      )
    )

    // trigger register
    core.io.trigger.assert := False
    core.io.trigger.transfer := False
    core.io.trigger.deassert := False
    rxFifo.io.flush := False
    txFifo.io.flush := False
    factory.setOnSet(core.io.trigger.assert, 0x1c, 0)
    factory.setOnSet(core.io.trigger.transfer, 0x1c, 1)
    factory.setOnSet(core.io.trigger.deassert, 0x1c, 2)
    factory.setOnSet(rxFifo.io.flush, 0x1c, 31)
    factory.setOnSet(txFifo.io.flush, 0x1c, 31)
    regs += Register(
      "trigger",
      0x1c,
      "Trigger actions to process",
      List(
        Field(
          "assert",
          Bool(),
          0 downto 0,
          AccessType.W1P,
          0,
          false,
          "Trigger assertion of CS",
          List(
            Value(0, "noop", "No action"),
            Value(1, "trigger", "Assert CS before other actions")
          )
        ),
        Field(
          "transceive",
          Bool(),
          1 downto 1,
          AccessType.W1P,
          0,
          false,
          "Tigger transceive of data in TX buffer",
          List(
            Value(0, "noop", "No action"),
            Value(1, "trigger", "Transceive data after possible CS assertion")
          )
        ),
        Field(
          "deassert",
          Bool(),
          2 downto 2,
          AccessType.W1P,
          0,
          false,
          "Trigger deassertion of CS",
          List(
            Value(0, "noop", "No action"),
            Value(1, "trigger", "Deassert CS after possible transceive of data")
          )
        ),
        Field(
          "flush",
          Bool(),
          31 downto 31,
          AccessType.W1P,
          0,
          false,
          "Flush TX/RX FIFOs",
          List(Value(0, "noop", "No action"), Value(1, "flush", "Flush FIFOs"))
        )
      )
    )

    // rx register
    rxFifo.io.push <> core.io.rxData.toStream
    factory.readStreamNonBlocking(rxFifo.io.pop, 0x20, 31, 0)
    regs += Register(
      "rx",
      0x20,
      "Gateway to RX FIFO",
      List(
        Field(
          "data",
          Bits(8 bits),
          7 downto 0,
          AccessType.RO,
          0,
          false,
          "Byte in RX buffer"
        ),
        Field(
          "validity",
          Bool(),
          31 downto 31,
          AccessType.RO,
          0,
          false,
          "Validity of data field",
          List(
            Value(0, "invalid", "Read data value is not valid, FIFO was empty"),
            Value(1, "valid", "Read data is valid")
          )
        )
      )
    )

    // tx register
    txFifo.io.push <> factory
      .createAndDriveFlow(Bits(p.core.datawidth bits), 0x24)
      .toStream
    core.io.txData <> txFifo.io.pop.addFragmentLast(txFifo.io.occupancy === 1)
    regs += Register(
      "tx",
      0x24,
      "Gateway to TX FIFO",
      List(
        Field(
          "data",
          Bits(8 bit),
          7 downto 0,
          AccessType.WO,
          0,
          false,
          "Enqueues data in the TX FIFO"
        )
      )
    )

    // guard times
    core.io.config.wordGuardClocks := factory.createReadAndWrite(
      UInt(p.core.wordGuardClocksWidth bits),
      0x28,
      0
    ) init 0
    core.io.config.csAssertGuard := factory.createReadAndWrite(
      UInt(p.core.csAssertGuardWidth bits),
      0x028,
      8
    ) init 1
    core.io.config.csDeassertGuard := factory.createReadAndWrite(
      UInt(p.core.csDeassertGuardWidth bits),
      0x28,
      16
    ) init 1
    regs += Register(
      "guard_times",
      0x28,
      "Guard times used during communication",
      List(
        Field(
          "word",
          UInt(8 bits),
          7 downto 0,
          AccessType.RW,
          0,
          false,
          "Number of clock durations of pause between individual sent bytes"
        ),
        Field(
          "assert",
          UInt(8 bits),
          15 downto 8,
          AccessType.RW,
          0,
          false,
          "Number of clock durations pause between SS assertion and transceive start"
        ),
        Field(
          "deassert",
          UInt(8 bits),
          23 downto 16,
          AccessType.RW,
          0,
          false,
          "Number of clock durations pause between transceive end and SS deassert"
        )
      )
    )

    override def elements = regs.toList
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
