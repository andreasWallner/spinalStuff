package andreasWallner.io.iomux

import andreasWallner.spinaltap.ISpinalTAPModule
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

case class IOMuxGenerics(
    inCnt: Int,
    outCnt: Int,
    lineCnt: Int,
    syncSteps: Int = 2
) {
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

  val synched = Vec(Bits(generics.lineCnt bits), generics.outCnt)
  for ((muxed, sync) <- io.muxeds.zip(synched))
    sync := Delay(muxed.read, generics.syncSteps)

  for ((out, outIdx) <- io.all.zipWithIndex) {
    for ((muxed, inIdx) <- synched.zip(io.sels)) {
      when(inIdx === U(outIdx)) {
        out.read := muxed
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
  for ((sel, idx) <- core.io.sels.zipWithIndex)
    sel := mapper.createReadAndWrite(
      UInt(generics.selWidth bits),
      0x04 * idx / 2,
      16 * (idx % 2)
    )

  def bus() = io.bus
}
