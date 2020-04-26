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
  def push(data: T, data_valid: Bool) = {
    valid.zip(buffer).foldLeft(when(False) {}) {
      case (expr, (v, b)) =>
        expr elsewhen (!v) {
          b := data
          v := data_valid
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
  val rx_buffer = ShortBuffer(Bits(16 bit), 4)

  val do_rx = Reg(Bool()) init(False)
  val do_tx = Reg(Bool()) init(False)
  val reg = new Area {
    val fx3 = new Area {
      val wr = Reg(Bool) init(False)
      val rd = RegNext(do_rx && io.fx3.empty_n && rx_buffer.empty) init (False)
      val oe = Reg(False) init (False)
      io.fx3.wr_n := !wr
      io.fx3.rd_n := !rd
      io.fx3.oe_n := !oe
      val dq = new Area {
        val write = Reg(Bits(16 bit))
        val writeEnable = Reg(Bool)

        io.fx3.dq.write := write
        io.fx3.dq.writeEnable := writeEnable
      }
    }
  }

  io.fx3.pktend_n := False

  // TODO never directly connect input: always go through short buffer on read? - or simply delay inputs by one?
  val rds = History(reg.fx3.rd, 3, init = False)
  io.rx.data.valid := False
  io.rx.data.payload := 0
  reg.fx3.oe := do_rx
  when(!io.fx3.empty_n) {
    rds.map(_.clear()) // TODO really leave this workaround in?
  }
  when(rds(2) && io.fx3.empty_n) {
    // data available && data accepted
    when(io.rx.data.ready && rx_buffer.empty) {
      // ready and not buffering?
      // then give it out
      io.rx.data.payload := io.fx3.dq.read
      io.rx.data.valid := True
    } otherwise {
      // otherwise, buffer it
      rx_buffer.push(io.fx3.dq.read, True)
    }
  } elsewhen (io.rx.data.ready && !rx_buffer.empty) {
    // buffer and ready to accept?
    // then shift out one buffered element
    io.rx.data.payload := rx_buffer.pop()
    io.rx.data.valid := True
  }

  val tx_buffer = ValidFlagShiftReg(Bits(16 bits), 4)
  val retransmit = Reg(tx_buffer.valid.clone()) init (0)
  val needs_retransmit = retransmit.orR
  io.tx.data.ready := False
  reg.fx3.dq.writeEnable := False
  reg.fx3.wr := False
  // TODO check generated HW - optimize code if necessary (duplicate statements like shift...)
  when(io.fx3.full_n) {
    when(do_tx && !needs_retransmit && io.tx.data.valid) {
      tx_buffer.shift(io.tx.data.payload, True)
      reg.fx3.dq.write := io.tx.data.payload
      io.tx.data.ready := True
      reg.fx3.wr := True
      reg.fx3.dq.writeEnable := True
    } elsewhen (do_tx && needs_retransmit) {
      reg.fx3.dq.write := tx_buffer.shift(0, False)
      reg.fx3.wr := retransmit(0)
      retransmit := False ## retransmit >> 1
      reg.fx3.dq.writeEnable := True
    } elsewhen (!needs_retransmit) {
      reg.fx3.dq.write := tx_buffer.shift(0, False)
    }
  } otherwise {
    retransmit := tx_buffer.valid
  }

  val dir = new Area {
    // TODO look for useless rx <-> tx changes
    // TODO explain why 5
    // TODO do lazy switching? only switch if other direction would transfer?
    val delay = Reg(Bits(5 bits)) init(0)
    val ready_to_tx = io.tx.en && (io.tx.data.valid || needs_retransmit) && io.fx3.full_n

    delay := delay(0 to 3) ## B"1"
    when(!do_tx && ready_to_tx) {
      do_rx := False
      when (delay(4)) {
        do_tx := delay(4)
        delay := 0
      }
    } elsewhen(!do_rx && !(ready_to_tx)) {
      do_tx := False
      when(delay(4)) {
        do_rx := True
        delay := 0
      }
    } otherwise {
      delay := 0
    }
  }
}
