package andreasWallner.io.iomux

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object IOMux {
  case class PortGenerics (
      triCnt: Int,
      outCnt: Int
  )
  case class MuxedPort(g: PortGenerics) extends Bundle with IMasterSlave {
    val tri = TriStateArray(g.triCnt)
    val o = Bits(g.outCnt bits)

    def asMaster() = {
      master(tri)
      out(o)
    }
  }

  case class Generics (
      inPorts: Int,
      outPorts: Int,
      portGenerics: PortGenerics,
      syncSteps: Int = 2,
      invertOutputEnable: Boolean = false
  ) {
    def selWidth = log2Up(inPorts)
  }

  case class Core(g: Generics) extends Component {
    val io = new Bundle {
      val all = Vec(slave(MuxedPort(g.portGenerics)), g.inPorts)
      val muxeds = Vec(master(MuxedPort(g.portGenerics)), g.outPorts)
      val sels = in Vec (UInt(g.selWidth bits), g.outPorts)
    }

    for ((muxed, sel) <- io.muxeds.zip(io.sels)) {
      muxed.tri.write := RegNext(io.all(sel).tri.write)
      muxed.tri.writeEnable := RegNext(io.all(sel).tri.writeEnable ^ B(g.portGenerics.triCnt bit, default -> g.invertOutputEnable))
      muxed.o := RegNext(io.all(sel).o)
    }

    for (a <- io.all)
      a.tri.read.clearAll()

    val synched = Vec(Bits(g.portGenerics.triCnt bits), g.outPorts)
    for ((muxed, sync) <- io.muxeds.zip(synched))
      sync := Delay(muxed.tri.read, g.syncSteps)

    for ((one, outIdx) <- io.all.zipWithIndex) {
      for ((muxed, inIdx) <- synched.zip(io.sels)) {
        when(inIdx === U(outIdx)) {
          one.tri.read := muxed
        }
      }
    }
  }

  class Ctrl[T <: spinal.core.Data with IMasterSlave](
      generics: Generics,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Component {
    val io = new Bundle {
      val bus = slave(busType())
      val all = Vec(slave(MuxedPort(generics.portGenerics)), generics.inPorts)
      val muxeds = Vec(master(MuxedPort(generics.portGenerics)), generics.outPorts)
    }
    val core = Core(generics)
    core.io.all <> io.all
    core.io.muxeds <> io.muxeds

    val mapper = factory(io.bus)
    for ((sel, idx) <- core.io.sels.zipWithIndex)
      sel := mapper.createReadAndWrite(
        UInt(generics.selWidth bits),
        0x04 * (idx / 4),
        8 * (idx % 4)
      ) init 0
  }
}
