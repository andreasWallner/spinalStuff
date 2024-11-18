package andreasWallner.misc

import andreasWallner.blackbox.xilinx.DNA_PORT
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.datamodel.BusComponent

import scala.language.postfixOps

case class XilinxDnaReader() extends Component {
  val io = new Bundle {
    val done = out Bool()
    val dna = out Bits(57 bit)
  }
  io.dna.setAsReg()

  val dna_port = new DNA_PORT(None);
  val cnt = Counter(57);
  val reset_done = RegInit(False);

  dna_port.READ := !reset_done;;
  dna_port.DIN := dna_port.DOUT;

  reset_done := True;
  when(reset_done && !cnt.willOverflowIfInc) {
    cnt.increment()
    io.dna := io.dna(56 downto 1) ## dna_port.DOUT;
  }
  io.done := cnt.willOverflowIfInc;
}

class BuildInfoPeripheral[T <: spinal.core.Data with IMasterSlave](
    infoData: List[String],
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory,
    withXilinxDna: Boolean = false
) extends Component
    with BusComponent {
  val io = new Bundle {
    val bus = slave(busType())
  }

  val mapper = metaFactory(io.bus)
  val f = new BusSlaveFactoryRecorder(mapper)

  val info_content = make_array(infoData.mkString(";"))
  val info_mem = Mem(info_content)

  val info_addr = Reg(UInt(log2Up(info_content.length) bits)) init 0
  val info_stream = Stream(UInt(f.dataWidth.toInt bits))
  info_stream.payload := info_mem.readSync(info_addr)
  info_stream.valid := True
  when(info_stream.ready) {
    when(info_addr =/= (info_content.length - 1)) {
      info_addr := info_addr + 1
    } otherwise {
      info_addr := 0
    }
  }

  val info = f.register(0, "info")
  info.read(
    info_stream.payload,
    0,
    "data",
    "Read register multiple times until 0 is read to get complete desription. (Content is MSB first)"
  )
  info_stream.ready := False
  mapper.onRead(0) { info_stream.ready := True }

  if(withXilinxDna) {
    val dna_reader = new XilinxDnaReader()

    val dna_0 = f.register(4, "dna_0")
    val dna_1 = f.register(8, "dna_1")
    dna_0.read(dna_reader.io.dna(57 downto 32), 0, "dna", "DNA bits [57:32")
    dna_0.read(dna_reader.io.done, 31, "valid", "DNA in registers is valid")
    dna_1.read(dna_reader.io.dna(31 downto 0), 0, "dna", "DNA bits [31:0]")
  }

  private def make_array(s: String): Array[UInt] = {
    if (f.dataWidth % 8 != 0)
      SpinalExit("can't create BuildInfoPeripheral for bus with width != n*8")
    val byteWidth = f.dataWidth / 8

    (
      List(U(0, f.dataWidth.toInt bits)) ++
        (for (group <- s.toList.grouped(4)) yield {
          var element = BigInt(0)
          for (i <- group.indices) {
            element = element | (BigInt(group(i)) << (8 * (byteWidth - 1 - i).toInt))
          }
          U(element, f.dataWidth.toInt bits)
        }) ++
        List(U(0, f.dataWidth.toInt bits))
    ).toArray
  }

  override def elements = f.elements
  override def busComponentName = "BuildInfo"
  override def dataWidth = f.dataWidth
  override def wordAddressInc = f.wordAddressInc
}
