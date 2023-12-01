package andreasWallner.zynq

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4WriteOnly}

case class HpFifoCtrl() extends Bundle with IMasterSlave {
  val rcount = UInt(8 bit)
  val wcount = UInt(8 bit)
  val racount = UInt(3 bit)
  val wacount = UInt(6 bit)
  val rdissuecap1_en = Bool()
  val wrissuecap1_en = Bool()

  override def asMaster(): Unit = {
    out(rcount, wcount, racount, wacount)
    in(rdissuecap1_en, wrissuecap1_en)
  }
}

case class HpWriteDma() extends Component {
  val io = new Bundle {
    val hp = master port Axi4(Axi4Config(32, 64, idWidth = 6))
    val fifoCtrl = slave port HpFifoCtrl()
    val data = slave port Stream(Bits(64 bit))
    val control = new Bundle {
      val en = in port Bool()
      val flush = in port Bool()
      val wrap = in port Bool()
      val startAddr = in port UInt(32 bit)
      val bufferSize = in port UInt(32 bit)
      val burstSize = in port Bits(3 bit)
    }
  }
  val burstLimit = io.control.burstSize
    .mux(
      0 -> U(0),
      1 -> U(1),
      2 -> U(3),
      3 -> U(7),
      4 -> U(15),
      5 -> U(31),
      6 -> U(63),
      7 -> U(127)
    )
    .resize(7 bit)
  val burstIncrement = io.control.burstSize
    .mux(
      0 -> U(1),
      1 -> U(2),
      2 -> U(4),
      3 -> U(8),
      4 -> U(16),
      5 -> U(32),
      6 -> U(64),
      7 -> U(128)
    )
    .resize(8 bit)

  val axi = Axi4WriteOnly(io.hp.config)
  axi >> io.hp

  val address = Reg(cloneOf(io.control.startAddr))
  val remaining = Reg(cloneOf(io.control.bufferSize))
  val last = remaining === burstIncrement
  val done = remaining === 0
  when(!io.control.en) {
    address := io.control.startAddr
    remaining := io.control.bufferSize
  } elsewhen (io.hp.aw.fire) {
    when(last && io.control.wrap) {
      address := io.control.startAddr
      remaining := io.control.bufferSize
    } otherwise {
      address := address + burstIncrement
      remaining := remaining - burstIncrement
    }
  }
  io.hp.aw.payload.addr := address
  io.hp.aw.valid.setAsReg().clearWhen(io.hp.aw.fire)

  io.hp.w.payload := axi.w.payload
  io.hp.w.valid := axi.w.valid & io.control.en
  axi.w.ready := io.hp.w.ready & io.control.en

  val burstProgress = Reg(UInt(8 bit)) init 0
  when(io.hp.w.fire) {
    when(burstProgress === burstLimit) {
      burstProgress := 0
      io.hp.aw.valid := True
    } otherwise {
      burstProgress := burstProgress + 1
    }
  }
}
