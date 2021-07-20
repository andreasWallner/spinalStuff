package andreasWallner.io.spi

import spinal.core.ClockDomain.FixedFrequency
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.language.postfixOps

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
      prescalerWidth: Int = 30,
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
    val prescaler = UInt(p.prescalerWidth bits)
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
      val counter = Reg(UInt(p.prescalerWidth bits))
      val phase = Reg(Bool())
      val guardCnt = Reg(UInt(p.wordGuardClocksWidth bit))

      when(!runTiming || counter === 0) {
        counter := io.config.prescaler
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
      val rxWord = Reg(Bits(p.datawidth bits)) init 0
      val rxReadyNext = False

      when(transferring) {
        when(timing.strobe) {
          clkActive := !clkActive
        }
        when(timing.update) {
          when(lastPhase) {
            when(io.config.msbFirst) {
              txWord := B"1" ## io.txData.payload.fragment.reversed
            } otherwise {
              txWord := B"1" ## io.txData.payload.fragment
            }
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
    when(io.config.msbFirst) { // TODO mux syntax
      io.rxData.payload := fsm.rxWord.reversed
    } otherwise {
      io.rxData.payload := fsm.rxWord
    }
  }

  class Ctrl[T <: spinal.core.Data with IMasterSlave](
      p: PeripheralParameter,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends Component {
    val io = new Bundle {
      val bus = slave(busType())
      val spi = master(Spi())
    }
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
    factory.read(U(frequency), 0x04, 0)

    factory.read(U(p.rxBufferSize), 0x08, 0)
    factory.read(U(p.txBufferSize), 0x08, 16)

    factory.read(U(p.core.prescalerWidth), 0x0c, 0)

    // status registers
    factory.read(core.io.busy, 0x10, 0)
    factory.doBitsAccumulationAndClearOnRead( // tx overflow?
      rxFifo.io.push.isStall.asBits,
      0x10,
      1
    )
    factory.read(rxFifo.io.occupancy, 0x14, 0)
    factory.read(txFifo.io.occupancy, 0x14, 16)

    // config register
    core.io.config.spiType.cpha := factory.createReadAndWrite(Bool, 0x18, 0)
    core.io.config.spiType.cpol := factory.createReadAndWrite(Bool, 0x18, 1)
    core.io.config.prescaler := factory.createReadAndWrite(
      UInt(p.core.prescalerWidth bits),
      0x18,
      2
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

    // rx register
    rxFifo.io.push <> core.io.rxData.toStream
    factory.readStreamNonBlocking(rxFifo.io.pop, 0x20, 31, 0)

    // tx register
    txFifo.io.push <> factory
      .createAndDriveFlow(Bits(p.core.datawidth bits), 0x24)
      .toStream
    core.io.txData <> txFifo.io.pop.addFragmentLast(txFifo.io.occupancy === 1)

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
