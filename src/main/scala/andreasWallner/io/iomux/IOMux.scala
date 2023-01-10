package andreasWallner.io.iomux

import andreasWallner.registers.casemodel.Value
import andreasWallner.registers.datamodel.BusComponent
import andreasWallner.registers.{BusSlaveFactoryRecorder, RegisterRecorder}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object IOMux {

  /**
    * @param triCnt Number of tristate signals to multiplex
    * @param outCnt Number of output signals to multiplex
    */
  case class PortGenerics(
      triCnt: Int,
      outCnt: Int
  )
  case class MuxedPort(g: PortGenerics) extends Bundle with IMasterSlave {
    val tri = TriStateArray(g.triCnt)
    val o = Bits(g.outCnt bits)

    def asMaster(): Unit = {
      master(tri)
      out(o)
    }
  }

  /**
    * @param inPorts Number of 'inside' ports to be multiplexed to outs
    * @param outPorts Number of multiplexed output ports
    * @param portGenerics Sizing of individual ports
    * @param syncSteps Length of synchronizier in input path
    * @param invertOutputEnable `true` to invert `writeEnable` signals of outputs
    * @param withSwap `true` to add additional layer to allow for swapping of tristate signals in input ports
    */
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

  /**
   * Multiplexer itself
   *
   * Connects one port from io.all with one port from io.muxeds, depending
   * on io.sels.
   * If g.withSwap is enabled, io.swapSel can be used to swap tristate
   * pins of each individual io.all port.
   */
  case class Core(g: Generics) extends Component {
    val io = new Bundle {
      val all = Vec(slave(MuxedPort(g.portGenerics)), g.inPorts)
      val muxeds = Vec(master(MuxedPort(g.portGenerics)), g.outPorts)
      val sels = in Vec (UInt(g.selWidth bits), g.outPorts)
      val swapSel =
        if (g.withSwap) Some(in(Vec(Vec(UInt(g.swapWidth bits), g.portGenerics.triCnt), g.inPorts)))
        else None
    }

    val swappeds = if (g.withSwap) {
      val swappeds = io.all.clone
      for ((input, swapped, sel) <- (io.all, swappeds, io.swapSel.get).zipped) {
        val inputWrites = for (i <- 0 until g.portGenerics.triCnt) yield (i, input.tri(i).write)
        val inputWriteEnables = for (i <- 0 until g.portGenerics.triCnt) yield (i, input.tri(i).writeEnable)
        val falseFill = for (i <- g.portGenerics.triCnt to g.swapMax) yield (i, False)
        val swappedReads = for (i <- 0 until g.portGenerics.triCnt) yield (i, swapped.tri(i).read)
        for (i <- 0 until g.portGenerics.triCnt) {
          swapped.tri(i).write := sel(i).muxListDc(inputWrites)
          swapped.tri(i).writeEnable := sel(i).muxList(
            inputWriteEnables ++ falseFill
          )
          input.tri(i).read := sel(i).muxListDc(swappedReads)
        }
        swapped.o := input.o
      }
      swappeds
    } else {
      io.all
    }

    for ((muxed, sel) <- (io.muxeds, io.sels).zipped) {
      muxed.tri.write := RegNext(swappeds(sel).tri.write)
      muxed.tri.writeEnable := RegNext(
        swappeds(sel).tri.writeEnable ^ B(
          g.portGenerics.triCnt bit,
          default -> g.invertOutputEnable
        )
      )
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

  /**
   * Bus connected IO multiplexer
   */
  case class Ctrl[T <: spinal.core.Data with IMasterSlave](
      generics: Generics,
      busType: HardType[T],
      factory: T => BusSlaveFactory,
      defaultPort: Int = 0
  ) extends Component with BusComponent {
    val io = new Bundle {
      val bus = slave(busType())
      val all = Vec(slave(MuxedPort(generics.portGenerics)), generics.inPorts)
      val muxeds =
        Vec(master(MuxedPort(generics.portGenerics)), generics.outPorts)
    }
    val core = Core(generics)
    core.io.all <> io.all
    core.io.muxeds <> io.muxeds

    val mapper = new BusSlaveFactoryRecorder(factory(io.bus))
    var register: RegisterRecorder = null// TODO improve this shameful thing
    for ((sel, idx) <- core.io.sels.zipWithIndex) {
      if(idx % 4 == 0) {
        val regIdx = idx / 4
        register = mapper.register(s"sel$regIdx", "Select multiplexer output")
      }
      sel := register.createReadAndWrite(
        UInt(generics.selWidth bits),
        8 * (idx % 4),
        s"out$idx"
      ) init defaultPort
    }

    if(generics.withSwap) {
      for((swapSels, inIdx) <- core.io.swapSel.get.zipWithIndex) {
        assert(swapSels.length * generics.swapWidth <= mapper.dataWidth, "currently not implemented to split to multiple registers")
        val register = mapper.register(s"swap$inIdx", s"Swap signals of input $inIdx")
        for((swapSel, swapIdx) <- swapSels.zipWithIndex) {
          swapSel := register.createReadAndWrite(
            UInt(generics.swapWidth bits),
            swapIdx * generics.swapWidth,
            s"tri$swapIdx",
            values=List(
              Value(swapSel.maxValue.toLong, "dis", "Disable driver")
            )
          ) init swapIdx
        }
      }
    }

    override def elements = mapper.elements
    override def dataWidth = mapper.dataWidth
    override def busComponentName = name
    override def wordAddressInc = mapper.wordAddressInc
  }
}
