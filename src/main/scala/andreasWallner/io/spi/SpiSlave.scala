package andreasWallner.io.spi

import spinal.core._
import spinal.lib._
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
    ret.cs := BufferCC(this.cs)
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

    val config = in port SpiType()
    val state = new Bundle {
      val busy = out port Bool().setAsReg().init(False)
    }
  }
  val sync = io.spi.synchronized()
  val normalizedClk = sync.sclk ^ io.config.cpol ^ io.config.cpha

  val shiftReg = Reg(Bits(9 bit))
  val bitCnt = Reg(UInt(log2Up(wordWidth.value) bit)) init 0

  val sample = normalizedClk.rise(True)
  val shift = normalizedClk.fall(False)
  val load = (!io.config.cpha && sync.cs.fall()) || (bitCnt === 0 && shift)

  val requestDrive = Reg(Bool()) init False
  sync.miso.writeEnable := !io.spi.cs // use unsynchronized

  sync.miso.write := shiftReg(0)
  io.rx.payload := shiftReg(8 downto 1)
  io.tx.ready := False

  val didLoad = Reg(Bool())
  requestDrive.setWhen(load).clearWhen(io.spi.cs)
  when(load) {
    shiftReg(7 downto 0) := io.tx.valid ? io.tx.payload | B(0)
    didLoad := io.tx.valid
    io.state.busy := True
    bitCnt := wordWidth.value - 1
  }

  when(shift && !load) {
    shiftReg(7 downto 0) := shiftReg(8 downto 1)
    bitCnt := bitCnt - 1
  }

  when(sample) {
    shiftReg(8) := sync.mosi
    when(bitCnt === 0) {
      io.state.busy := False
      io.tx.ready := didLoad
    }
  }
  io.rx.valid := RegNext(sample && bitCnt === 0) init False
}
