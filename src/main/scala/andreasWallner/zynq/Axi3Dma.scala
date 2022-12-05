package andreasWallner.zynq

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4WriteOnly}

import scala.language.postfixOps


case class DmaConfig(axiConfig: Axi4Config) extends Bundle {
  val startAddress = UInt(axiConfig.addressWidth bit)
  val endAddress = UInt(axiConfig.addressWidth bit)
  val circular = Bool()
}

case class Axi3Dma(axiConfig: Axi4Config = Axi4Config(addressWidth = 64, dataWidth = 64, idWidth = 2)) extends Component {
  val io = new Bundle {
    val axi = master(Axi4WriteOnly(axiConfig)) // use Axi4 for now, close enough (see http://www.vlsiip.com/amba/axi34.html)
    val config = in(DmaConfig(axiConfig))
    val data = slave(Stream(Bits(16 bit)))
    val dataAvailable = in(UInt(8 bit))
    val run = in(Bool())
    val hasWrapped = out(Bool()) setAsReg()
    val busy = out(Bool())
    val full = out(Bool()) setAsReg()
  }
  val outstandingB = Reg(UInt(3 bit)) init 0
  val awDone = Reg(Bool()) init False
  val wDone = Reg(Bool()) init False

  io.busy := (io.data.valid || awDone || wDone || outstandingB =/= 0) & !io.full

  val address = Reg(UInt(axiConfig.addressWidth bits))
  when(!io.run) {
    address := io.config.startAddress
    io.hasWrapped := False
    awDone := False
    wDone := False
    io.full := False
  } otherwise {
    when(io.axi.aw.fire) {
      awDone := True
    }
    when(io.axi.w.fire) {
      wDone := True
    }
  }

  outstandingB := (io.axi.aw.fire ## io.axi.b.fire).mux(
    B"10" -> (outstandingB + 1),
    B"01" -> (outstandingB - 1),
    default -> outstandingB
  )

  when((awDone && io.axi.w.fire) || (wDone && io.axi.aw.fire) || (io.axi.aw.fire && io.axi.w.fire)) {
    awDone := False
    wDone := False

    when(address + 2 === io.config.endAddress) {
      when(!io.config.circular) {
        io.full := True
      }
      io.hasWrapped := True
      address := io.config.startAddress
    } otherwise {
      address := address + 2
    }
  }

  io.axi.aw.payload.addr := address
  // TODO fix issue with last transfer not happening of W channel
  // transacts first (and io.data.valid goes away) by delaying
  // the data.ready signal and not make be tied to W.ready but
  // awDone & wDone?
  io.axi.aw.valid := io.run & ((io.data.valid || wDone) & !awDone) & !io.full // TODO NEEDED? & outstandingB =/= outstandingB.maxValue

  io.axi.w.payload.data := io.data.payload #* 4
  io.axi.w.payload.strb := address(2 downto 1).mux(
    0 -> B"11".resized,
    1 -> B"1100".resized,
    2 -> B"110000".resized,
    3 -> B"11000000".resized
  )
  io.axi.w.valid := io.data.valid & io.run & !wDone & !io.full
  io.data.ready := io.axi.w.ready & io.run & !wDone & !io.full

  io.axi.b.ready := True

  // AXI AW dummy settings
  if (axiConfig.useLen) io.axi.aw.payload.len := 0
  io.axi.aw.setSize(1)
  if (axiConfig.useId) io.axi.aw.payload.id := 0
  if (axiConfig.useRegion) io.axi.aw.payload.region := 0
  io.axi.aw.setLock(0)
  io.axi.aw.setCache(0) // TODO allow to be buffered?
  if (axiConfig.useQos) io.axi.aw.qos := 0
  if (axiConfig.useAwUser) io.axi.aw.payload.user := 0
  if (axiConfig.useProt) io.axi.aw.payload.prot := B"011"
  if (axiConfig.useBurst) io.axi.aw.payload.burst := B"00"

  if (axiConfig.useWUser) io.axi.w.payload.user := 0
  if (axiConfig.useLast) io.axi.w.payload.last := True
}
