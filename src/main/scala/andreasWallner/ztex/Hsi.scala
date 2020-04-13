package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.lib.History

case class FX3() extends Bundle with IMasterSlave {
  val dq = TriState(Bits(16 bit))
  val wr_n = out Bool
  val rd_n = out Bool
  val oe_n = out Bool

  val empty_n = in Bool
  val full_n = in Bool

  val pktend_n = out Bool

  def asMaster() = {
    master(dq)
    in(empty_n, full_n)
    out(wr_n, rd_n, oe_n, pktend_n)
  }
}

case class ShortBuffer[T <: Data](val dataType: HardType[T], val depth: Int)
    extends Area {
  val buffer = Vec(Reg(dataType), depth)
  val valid = Vec(Reg(Bool), depth)
  valid.map(_.init(False))

  // TODO: we would not necessarily need a elsewhen on the last entry, might save minimal hardware is it worth it?
  def push(data: T) = {
    valid.zip(buffer).foldLeft(when(False) {}) {
      case (expr, (v, b)) =>
        expr elsewhen (!v) {
          b := data
          v := True
        }
    }
  }

  def pop(): T = {
    val output = cloneOf(dataType)
    output := buffer(0)
    valid(depth - 1) := False

    for (i <- (1 to depth - 1).reverse) {
      buffer(i - 1) := buffer(i)
      valid(i - 1) := valid(i)
    }

    output
  }

  def empty: Bool = !valid(0)
}

case class ValidFlagShiftReg[T <: Data](
    val dataType: HardType[T],
    val depth: Int
) extends Area {
  val buffer = Vec(Reg(dataType), depth)
  val valid = Reg(Bits(depth bits)) init (0)

  def shift(data: T, validFlag: Bool = True): T = {
    val output = cloneOf(dataType)
    output := buffer(0)

    for (i <- 1 to depth - 1)
      buffer(i - 1) := buffer(i)
    buffer(depth - 1) := data
    valid := validFlag ## valid >> 1

    output
  }
}

case class HsiInterface() extends Component {
  val io = new Bundle {
    val fx3 = master(FX3())
    val tx = new Bundle {
      val data = slave(Stream(Bits(16 bit)))
      val en = in Bool
      val pktend = in Bool
      val pktend_timeout = in UInt (16 bits)
    }
    val rx = new Bundle {
      val data = master(Stream(Bits(16 bit)))
    }
  }
  val rx_buffer = ShortBuffer(Bits(16 bit), 3)

  val do_rx = RegNext(!io.tx.en) init (False)
  val do_tx = RegNext(io.tx.en) init (False)
  val reg = new Area {
    val wr = Bool
    val rd = do_rx && io.fx3.empty_n && rx_buffer.empty
    val oe = Reg(False) init (False)

    io.fx3.wr_n := !wr
    io.fx3.rd_n := !rd
    io.fx3.oe_n := !oe
  }

  io.fx3.dq.write := 0
  io.fx3.pktend_n := False

  val rds = History(reg.rd, 4, init = False)
  io.rx.data.valid := False
  io.rx.data.payload := 0
  reg.oe := do_rx
  when(rds(2) && io.fx3.empty_n) {
    // data available && data accepted
    when(io.rx.data.ready && rx_buffer.empty) {
      // ready and not buffering?
      // then give it out
      io.rx.data.payload := io.fx3.dq.read
      io.rx.data.valid := True
    } otherwise {
      // otherwise, buffer it
      rx_buffer.push(io.fx3.dq.read)
    }
  } elsewhen (io.rx.data.ready && !rx_buffer.empty) {
    // buffer and ready to accept?
    // then shift out one buffered element
    io.rx.data.payload := rx_buffer.pop()
    io.rx.data.valid := True
  }

  val tx_buffer = ValidFlagShiftReg(Bits(16 bits), 3)
  val retransmit = Reg(Bits(3 bits)) init (0)
  val needs_retransmit = retransmit.orR
  io.tx.data.ready := False
  io.fx3.dq.writeEnable := do_tx
  reg.wr := False
  when(do_tx) {
    when(io.fx3.full_n) {
      when(!needs_retransmit && io.tx.data.valid) {
        tx_buffer.shift(io.tx.data.payload, True)
        io.fx3.dq.write := io.tx.data.payload
        io.tx.data.ready := True
        reg.wr := True
      } elsewhen (needs_retransmit) {
        io.fx3.dq.write := tx_buffer.shift(0, False)
        reg.wr := retransmit(0)
        retransmit := False ## retransmit >> 1
      } otherwise {
        tx_buffer.shift(0, False)
      }
    } otherwise {
      retransmit := tx_buffer.valid
    }
  }
}
