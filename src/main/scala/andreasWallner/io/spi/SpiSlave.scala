package andreasWallner.io.spi

import andreasWallner.registers.BusSlaveFactoryRecorder
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateOutput

import scala.language.postfixOps

case class SpiSlave() extends Bundle with IMasterSlave {
  val sclk = Bool()
  val mosi = Bool()
  val miso = TriStateOutput(Bool())
  val cs = Bool()

  override def asMaster(): Unit = {
    in(sclk, mosi, cs)
    master(miso)
  }

  def synchronized() = {
    val ret = cloneOf(this)
    ret.sclk := BufferCC(this.sclk)
    ret.mosi := BufferCC(this.mosi)
    ret.cs := BufferCC(this.cs, init=True)
    this.miso.write := ret.miso.write
    this.miso.writeEnable := ret.miso.writeEnable
    ret
  }
}

case class SpiSlaveController(wordWidth: BitCount) extends Component {
  val io = new Bundle {
    val spi = master port SpiSlave()
    val tx = slave port Stream(Bits(wordWidth))
    val rx = master port Flow(Bits(wordWidth))

    val en = in port Bool()

    val config = in port SpiType()
    val status = new Bundle {
      val busy = out port Bool().setAsReg().init(False)
      val selected = out port Bool()
    }
  }
  val sync = io.spi.synchronized()
  val normalizedClk = sync.sclk ^ io.config.cpol ^ io.config.cpha
  io.status.selected := !sync.cs

  val shiftReg = Reg(Bits(9 bit))
  val bitCnt = Reg(UInt(log2Up(wordWidth.value) bit)) init 0

  val sample = normalizedClk.rise(True)
  val shift = normalizedClk.fall(False)
  val load = (!io.config.cpha && sync.cs.fall()) || (bitCnt === 0 && shift)

  sync.miso.writeEnable := !io.spi.cs // use unsynchronized

  sync.miso.write := shiftReg(0)
  io.tx.ready := False
  // use current mosi, updated register would a cycle be too late
  io.rx.payload := sync.mosi ## shiftReg(7 downto 1)
  io.rx.valid := False

  // remember if there was data available when starting the byte
  // to only ack if there was data originally and not loose a byte
  val didLoad = Reg(Bool()).init(False)
  when(load && io.en && io.status.selected) {
    shiftReg(7 downto 0) := io.tx.valid ? io.tx.payload | B(0)
    didLoad := io.tx.valid
    io.status.busy := True
    bitCnt := wordWidth.value - 1
  }

  when(shift && !load && io.status.busy) {
    shiftReg(7 downto 0) := shiftReg(8 downto 1)
    bitCnt := bitCnt - 1
  }

  when(sample && io.status.busy) {
    shiftReg(8) := sync.mosi
    when(bitCnt === 0) {
      io.status.busy := False
      io.tx.ready := didLoad
      io.rx.valid := True
    }
  }
}

case class SpiSlavePeripheral[T <: Data with IMasterSlave](
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component {
  val io = new Bundle {
    val spi = master port SpiSlave()
    val bus = slave(busType())
  }
  val flush = False
  val rxFifo = StreamFifo(Bits(8 bit), 16)
  val txFifo = StreamFifo(Bits(8 bit), 16)
  val core = SpiSlaveController(8 bit)
  io.spi <> core.io.spi
  txFifo.io.pop >> core.io.tx
  txFifo.io.flush := flush
  rxFifo.io.flush := flush
  val rxFifoIsOverflow = Bool()
  core.io.rx.toStream(rxFifoIsOverflow) >> rxFifo.io.push

  val factory = new BusSlaveFactoryRecorder(metaFactory(io.bus))

  val status = factory.register("status")
  val rxFifoOverflowFlag = Reg(Bool()) init False
  status.read(core.io.status.busy, 0, "busy", "A byte is being transmitted")
  status.read(core.io.status.selected, 1, "selected", "CS is asserted")
  status.read(rxFifo.io.occupancy =/= 0, 2, "rxne", "RX FIFO is not empty")
  status.read(rxFifo.io.availability =/= 0, 3, "rxnf", "RX FIFO is not full")
  status.driveAndRead(rxFifoOverflowFlag, 4, "rxovfl", "RX FIFO overflowed")
  status.read(txFifo.io.availability =/= 0, 5, "txnf", "TX FIFO is not full")
  status.read(rxFifo.io.occupancy =/= 0, 6, "txne", "TX FIFO is not empty")
  status.read(rxFifo.io.occupancy, 16, "rx_fill", "# of bytes in RX FIFO")
  status.read(txFifo.io.availability, 24, "tx_fill", "# of unused bytes in TX FIFO")
  rxFifoOverflowFlag.setWhen(rxFifoIsOverflow)

  val ctrl = factory.register("ctrl")
  ctrl.driveAndRead(core.io.en, 0, "en").init(False)
  ctrl.driveAndRead(core.io.config.cpha, 1, "cpha").init(False)
  ctrl.driveAndRead(core.io.config.cpol, 2, "cpol").init(False)
  ctrl.setOnSet(flush, 31, "flush", "discard FIFO contents")
  ctrl.clearOnSet(rxFifoOverflowFlag, 30, "clear_rxovfl", "clear RX overflow flag")

  val tx = factory.register("tx")
  val txFlow = tx.createAndDriveFlow(Bits(8 bit), 0, "data", "write to TX FIFO")
  txFlow.toStream >> txFifo.io.push

  val rx = factory.register("rx")
  rx.readStreamNonBlocking(rxFifo.io.pop, 31, 0, "data", "read from RX FIFO")
}
