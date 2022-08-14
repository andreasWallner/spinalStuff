package andreasWallner.misc

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.BusSlaveFactory
import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.datamodel.BusComponent

class BuildInfoPeripheral[T <: spinal.core.Data with IMasterSlave](
    infoData: List[String],
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
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
