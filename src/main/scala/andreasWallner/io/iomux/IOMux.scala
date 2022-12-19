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

  case class Generics(
                       inPorts: Int,
                       outPorts: Int,
                       portGenerics: PortGenerics,
                       syncSteps: Int = 2,
                       invertOutputEnable: Boolean = false,
                       withSwap: Boolean = false
                     ) {
    def selWidth = log2Up(inPorts)

    def swapWidth = log2Up(portGenerics.triCnt + 1)

    def swapMax = ((BigInt(1) << swapWidth) - 1).intValue()
  }

  case class Core(g: Generics) extends Component {
    val io = new Bundle {
      val all = Vec(slave(MuxedPort(g.portGenerics)), g.inPorts)
      val muxeds = Vec(master(MuxedPort(g.portGenerics)), g.outPorts)
      val sels = in Vec(UInt(g.selWidth bits), g.outPorts)
      val swapSel = if (g.withSwap) Some(in(Vec(Vec(UInt(g.swapWidth bits), g.portGenerics.triCnt), g.inPorts))) else None
    }

    val swappeds = g.withSwap match {
      case false => io.all
      case true =>
        val swappeds = io.all.clone
        for ((input, swapped, sel) <- (io.all, swappeds, io.swapSel.get).zipped) {
          val inputWrites = for (i <- 0 until g.portGenerics.triCnt) yield (i, input.tri(i).write)
          val inputWriteEnables = for (i <- 0 until g.portGenerics.triCnt) yield (i, input.tri(i).writeEnable)
          val falseFill = for (i <- g.portGenerics.triCnt to g.swapMax) yield (i, False)
          val swappedReads = for (i <- 0 until g.portGenerics.triCnt) yield (i, swapped.tri(i).read)
          for (i <- 0 until g.portGenerics.triCnt) {
            swapped.tri(i).write := sel(i).muxListDc(inputWrites)
            swapped.tri(i).writeEnable := sel(i).muxList(inputWriteEnables ++ falseFill)
            input.tri(i).read := sel(i).muxListDc(swappedReads)
          }
          swapped.o := input.o
        }
        swappeds
    }

    for ((muxed, sel) <- (io.muxeds, io.sels).zipped) {
      muxed.tri.write := RegNext(swappeds(sel).tri.write)
      muxed.tri.writeEnable := RegNext(swappeds(sel).tri.writeEnable ^ B(g.portGenerics.triCnt bit, default -> g.invertOutputEnable))
      muxed.o := RegNext(swappeds(sel).o)
    }

    for (a <- swappeds)
      a.tri.read.clearAll()

    val synced = Vec(Bits(g.portGenerics.triCnt bits), g.outPorts)
    for ((muxed, sync) <- (io.muxeds, synced).zipped)
      sync := Delay(muxed.tri.read, g.syncSteps)

    for ((one, outIdx) <- swappeds.zipWithIndex) {
      for ((muxed, inIdx) <- (synced, io.sels).zipped) {
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
