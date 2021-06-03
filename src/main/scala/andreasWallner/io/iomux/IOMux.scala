package andreasWallner.io.iomux

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriState
import spinal.lib.io.TriStateArray

case class IOMuxGenerics(inCnt: Int, outCnt: Int, lineCnt: Int) {
  def selWidth = log2Up(inCnt)
}

case class IOMux(generics: IOMuxGenerics) extends Component {
  val io = new Bundle {
    val all = Vec(slave(TriStateArray(generics.lineCnt)))
    val muxeds = Vec(master(TriStateArray(generics.lineCnt)))
    val sels = in Vec (UInt(generics.selWidth bits), generics.outCnt)
  }

  (io.muxeds, io.sels).zipped.foreach {
    case (muxed, sel) =>
      muxed.write := RegNext(io.all(sel).write)
      muxed.writeEnable := RegNext(io.all(sel).writeEnable)
  }
  io.all.zipWithIndex.foreach {
    case (out, outIdx) =>
      (io.sels, io.muxeds).zipped.foreach {
        case (sel, muxed) =>
          when(sel === U(outIdx)) {
            out.read := muxed.read
          }
      }
  }
}

abstract class IOMuxPeripheral[T <: spinal.core.Data with IMasterSlave](
  generics: IOMuxGenerics,
  busType: HardType[T],
  factory: T => BusSlaveFactory 
) extends Component {
  val io = new Bundle {
    val bus = slave(busType())
    val all = Vec(slave(TriStateArray(generics.lineCnt)))
    val muxeds = Vec(master(TriStateArray(generics.lineCnt)))
  }
  val core = IOMux(generics)
  core.io.all <> io.all
  core.io.muxeds <> io.muxeds

  val mapper = factory(io.bus)
  for (idx <- 1 to generics.outCnt) {
    core.io.sels(idx) := mapper.createReadAndWrite(UInt(generics.selWidth bits), 0x04 * idx / 2, 16 * (idx % 2))
  }
}