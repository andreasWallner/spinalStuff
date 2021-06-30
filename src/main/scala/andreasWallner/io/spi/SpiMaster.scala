package andreasWallner.io.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

import scala.language.postfixOps

// TODO CS control (bit that can be set with tx trigger) with timing control of offsets

case class SpiType() extends Bundle {
  val cpol = Bool
  val cpha = Bool
}

case class Spi() extends Bundle with IMasterSlave {
  val sclk = Bool()
  val mosi = Bool()
  val miso = Bool()

  override def asMaster(): Unit = {
    out(sclk, mosi)
    in(miso)
  }
}

object SpiMaster {
  case class CoreParameter(prescalerWidth: Int = 30, datawidth: Int = 8) {}

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
  }

  case class Core(p: CoreParameter = CoreParameter()) extends Component {
    val io = new Bundle {
      val spi = master(Spi())
      val config = in(CoreConfig(p))
      val start = in Bool
      val busy = out Bool
      val txData = slave(Stream(Fragment(Bits(p.datawidth bit))))
      val rxData = master(Flow(Bits(p.datawidth bits)))
    }

    val sync = new Area {
      val miso = BufferCC(io.spi.miso)
    }
    val reg = new Area {
      val sclk = Reg(Bool)
      io.spi.sclk := sclk
    }

    val transferring = Reg(Bool) init False
    io.busy := transferring
    val timing = new Area {
      val strobe = False
      val counter = Reg(UInt(p.prescalerWidth bits))
      val stateCnt = Reg(UInt(8 bit)) // TODO: smaller size

      when(!transferring) {
        counter := 0
        stateCnt := io.config.spiType.cpha.asUInt.resized
      } otherwise {
        when(counter === io.config.prescaler) {
          counter := 0
          strobe := True
        } otherwise {
          counter := counter + 1
        }
      }

      val lastState = stateCnt === ((8 << 1) - 1)
      val firstState = stateCnt === 0
      when(strobe) {
        when(lastState) {
          stateCnt := 0
        } otherwise {
          stateCnt := stateCnt + 1
        }
      }

      val isSample = !stateCnt(0) ^ io.config.spiType.cpha
      val sample = isSample && strobe
      val update = !isSample && strobe
    }
    io.txData.ready := False
    val fsm = new Area {

      val txWord = Reg(Bits(p.datawidth bits))
      val rxWord = Reg(Bits(p.datawidth bits)) init 0
      val rxReadyNext = False
      val lastWord = Reg(Bool)

      val loadNext = (timing.firstState || timing.lastState) && timing.update

      when(!transferring && io.start && io.txData.valid) {
        transferring := True
        txWord := io.txData.payload.fragment
        lastWord := io.txData.payload.last
        io.txData.ready := True
      }

      when(transferring) {
        when(timing.strobe && !(lastWord && timing.lastState)) {
          reg.sclk := !reg.sclk
        }
        when(timing.update) {
          txWord := txWord(0 to p.datawidth - 2) ## False
        }
        when(timing.sample) {
          rxWord := rxWord(0 to p.datawidth - 2) ## sync.miso
        }
        when(loadNext) {
          txWord := io.txData.payload.fragment
          lastWord := io.txData.payload.last
          io.txData.ready := True
        }
        when(timing.lastState && timing.strobe) {
          rxReadyNext := True
          when(lastWord) {
            transferring := False
          }
        }
      } otherwise {
        reg.sclk := io.config.spiType.cpol
      }
    }
    io.rxData.valid := RegNext(fsm.rxReadyNext) init False
    io.spi.mosi := fsm.txWord(p.datawidth - 1)
    io.rxData.payload := fsm.rxWord
  }

  abstract class Ctrl[T <: spinal.core.Data with IMasterSlave](
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

    factory.read(U(clockDomain.frequency.getValue.toLong), 0x04, 0)

    factory.read(U(p.rxBufferSize), 0x08, 0)
    factory.read(U(p.txBufferSize), 0x08, 16)

    factory.read(U(p.core.prescalerWidth), 0x0c, 0)

    // status registers
    factory.read(core.io.busy, 0x10, 0)
    factory.doBitsAccumulationAndClearOnRead(
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
    core.io.start := False
    rxFifo.io.flush := False
    txFifo.io.flush := False
    factory.setOnSet(core.io.start, 0x1c, 0)
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
