package andreasWallner.io.iomux

import andreasWallner.spinaltap.ISpinalTAPModule
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriState
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

case class IOMuxGenerics(inCnt: Int, outCnt: Int, lineCnt: Int) {
  def selWidth = log2Up(inCnt)
}

case class IOMux(generics: IOMuxGenerics) extends Component {
  val io = new Bundle {
    val all = Vec(slave(TriStateArray(generics.lineCnt)), generics.inCnt)
    val muxeds = Vec(master(TriStateArray(generics.lineCnt)), generics.outCnt)
    val sels = in Vec (UInt(generics.selWidth bits), generics.outCnt)
  }

  for ((muxed, sel) <- io.muxeds.zip(io.sels)) {
    muxed.write := RegNext(io.all(sel).write)
    muxed.writeEnable := RegNext(io.all(sel).writeEnable)
  }

  for (a <- io.all)
    a.read.clearAll()

  for ((out, outIdx) <- io.all.zipWithIndex) {
    for ((muxed, inIdx) <- io.muxeds.zip(io.sels)) {
      when(inIdx === U(outIdx)) {
        out.read := muxed.read
      }
    }
  }
}

class IOMuxPeripheral[T <: spinal.core.Data with IMasterSlave](
    generics: IOMuxGenerics,
    busType: HardType[T],
    factory: T => BusSlaveFactory
) extends Component
    with ISpinalTAPModule[T] {
  val io = new Bundle {
    val bus = slave(busType())
    val all = Vec(slave(TriStateArray(generics.lineCnt)), generics.inCnt)
    val muxeds = Vec(master(TriStateArray(generics.lineCnt)), generics.outCnt)
  }
  val core = IOMux(generics)
  core.io.all <> io.all
  core.io.muxeds <> io.muxeds

  val mapper = factory(io.bus)
  for (idx <- 1 until generics.outCnt) {
    core.io.sels(idx) := mapper.createReadAndWrite(
      UInt(generics.selWidth bits),
      0x04 * idx / 2,
      16 * (idx % 2)
    )
  }

  def bus() = io.bus
}
