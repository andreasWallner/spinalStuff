package andreasWallner.io.iomux

import andreasWallner.spinaltap.ISpinalTAPModule
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object IOMux {
  case class Parameter(
      inCnt: Int,
      outCnt: Int,
      lineCnt: Int,
      syncSteps: Int = 2
  ) {
    def selWidth = log2Up(inCnt)
  }

  case class Core(p: Parameter) extends Component {
    val io = new Bundle {
      val all = Vec(slave(TriStateArray(p.lineCnt)), p.inCnt)
      val muxeds = Vec(master(TriStateArray(p.lineCnt)), p.outCnt)
      val sels = in Vec (UInt(p.selWidth bits), p.outCnt)
    }

    for ((muxed, sel) <- io.muxeds.zip(io.sels)) {
      muxed.write := RegNext(io.all(sel).write)
      muxed.writeEnable := RegNext(io.all(sel).writeEnable)
    }

    for (a <- io.all)
      a.read.clearAll()

    val synched = Vec(Bits(p.lineCnt bits), p.outCnt)
    for ((muxed, sync) <- io.muxeds.zip(synched))
      sync := Delay(muxed.read, p.syncSteps)

    for ((out, outIdx) <- io.all.zipWithIndex) {
      for ((muxed, inIdx) <- synched.zip(io.sels)) {
        when(inIdx === U(outIdx)) {
          out.read := muxed
        }
      }
    }
  }

  class Ctrl[T <: spinal.core.Data with IMasterSlave](
      generics: Parameter,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Component
      with ISpinalTAPModule[T] {
    val io = new Bundle {
      val bus = slave(busType())
      val all = Vec(slave(TriStateArray(generics.lineCnt)), generics.inCnt)
      val muxeds = Vec(master(TriStateArray(generics.lineCnt)), generics.outCnt)
    }
    val core = Core(generics)
    core.io.all <> io.all
    core.io.muxeds <> io.muxeds

    val mapper = factory(io.bus)
    for ((sel, idx) <- core.io.sels.zipWithIndex)
      sel := mapper.createReadAndWrite(
        UInt(generics.selWidth bits),
        0x04 * idx / 4,
        8 * (idx % 4)
      ) init 0

    def bus() = io.bus
  }
}
