package andreasWallner.io.fx3

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class SlaveFifo() extends Bundle with IMasterSlave {
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

/** Buffer to provide incoming data as Stream
  *
  * Provides a shift-register to push data into and mux & logic to get data
  * out as in a FIFO.
  *
  * We want this to be directly connected to the input pins, therefore we
  * want no logic between the data input and the register.
  * This is the reason why no standard SpinalHDL FIFO can be used.
  * Also the reason why this is written inefficiently w.r.t. hardware size:
  * it could be smaller if the input was muxed and the output shifted, but
  * that would lead to LUTs before the first register.
  */
case class ShortBuffer[T <: Data](val dataType: HardType[T], val depth: Int)
    extends Component {
  require(depth >= 1)
  val io = new Bundle {
    val push = new Bundle {
      val data = in(dataType())
      val en = in Bool ()
    }
    val pop = master Stream (dataType)
    val willBecomeEmpty = out Bool ()
  }

  val buffer = Vec(Reg(dataType), depth)
  val valid = Vec(Reg(False), depth + 1)
  valid.map(_.init(False))
  valid(depth) := False

  val newValid = Vec(Bool, depth)
  when(io.push.en) {
    buffer(0) := io.push.data
    valid(0) := True
    for (i <- 0 until depth - 1) {
      buffer(i + 1) := buffer(i)
      valid(i + 1) := newValid(i)
    }
  } otherwise {
    valid.zip(newValid).map {
      case (v, nv) =>
        v := nv
    }
  }
  io.willBecomeEmpty := !valid(0) || (!valid(1) && io.pop.ready)
  for (i <- newValid.range) {
    newValid(i) := Mux(
      !(io.pop.ready && io.pop.valid),
      valid(i),
      valid(i) && valid(i + 1)
    )
  }
  io.pop.valid := valid.orR
  val validOH = OHMasking.last(valid.asBits(0 until depth))
  io.pop.payload := MuxOH(validOH, buffer)
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

case class SlaveFifoMaster() extends Component {
  val io = new Bundle {
    val fx3 = master(SlaveFifo())
    val tx = new Bundle {
      val data = slave(Stream(Bits(16 bit)))
      val en = in Bool
      val pktend = in Bool
      val pktend_timeout = in UInt (16 bits)
      val pktend_done = out Bool
    }
    val rx = new Bundle {
      val data = master(Stream(Bits(16 bit)))
    }
  }

  val do_rx = Reg(Bool()) init (False)
  val do_tx = Reg(Bool()) init (False)
  val reg = new Area {
    val fx3 = new Area {
      val rd_n = Reg(Bool) init True
      val wr_n = Reg(Bool) init True
      val oe_n = Reg(Bool) init True
      val pktend_n = Reg(Bool) init True

      // TODO remove inversion after register?
      io.fx3.wr_n := wr_n
      io.fx3.rd_n := rd_n
      io.fx3.oe_n := oe_n
      io.fx3.pktend_n := pktend_n
      val dq = new Area {
        val write = Reg(Bits(16 bit))
        val writeEnable = Reg(Bool)

        io.fx3.dq.write := write
        io.fx3.dq.writeEnable := writeEnable
      }
    }
  }

  reg.fx3.oe_n := !do_rx

  val rx = new Area {
    val rds = History(!reg.fx3.rd_n, 3, init = False)
    val buffer = ShortBuffer(Bits(16 bit), 4)
    reg.fx3.rd_n := !(do_rx && io.fx3.empty_n && buffer.io.willBecomeEmpty)
    io.rx.data <> buffer.io.pop
    buffer.io.push.data := io.fx3.dq.read
    buffer.io.push.en := rds(2) && io.fx3.empty_n

    when(!io.fx3.empty_n) {
      rds.map(_.clear()) // TODO really leave this workaround in?
    }
  }


  val tx = new Area {
    val buffer = ValidFlagShiftReg(Bits(16 bits), 4)
    val retransmit = Reg(buffer.valid.clone()) init (0)
    val needs_retransmit = retransmit.orR
    io.tx.data.ready := False
    reg.fx3.dq.writeEnable := False
    reg.fx3.wr_n := True
    // TODO check generated HW - optimize code if necessary (duplicate statements like shift...)
    when(io.fx3.full_n) {
      when(do_tx && !needs_retransmit && io.tx.data.valid) {
        buffer.shift(io.tx.data.payload, True)
        reg.fx3.dq.write := io.tx.data.payload
        io.tx.data.ready := True
        reg.fx3.wr_n := False
        reg.fx3.dq.writeEnable := True
      } elsewhen (do_tx && needs_retransmit) {
        reg.fx3.dq.write := buffer.shift(0, False)
        reg.fx3.wr_n := !retransmit(0)
        retransmit := False ## retransmit >> 1
        reg.fx3.dq.writeEnable := True
      } elsewhen (!needs_retransmit) {
        reg.fx3.dq.write := buffer.shift(0, False)
      }
    } otherwise {
      retransmit := buffer.valid
    }
  }

  val dir = new Area {
    // TODO look for useless rx <-> tx changes
    // TODO explain why 5
    // TODO do lazy switching? only switch if other direction would transfer?
    // TODO should we really keep transmitting as long as retransmit is set? does _not_ doing so make sense?
    val delay = Reg(Bits(5 bits)) init (0)
    val ready_to_tx = (io.tx.en || tx.needs_retransmit) && (io.tx.data.valid || tx.needs_retransmit) && io.fx3.full_n

    delay := delay(0 to 3) ## B"1"
    when(!do_tx && ready_to_tx) {
      do_rx := False
      when(delay(4)) {
        do_tx := delay(4)
        delay := 0
      }
    } elsewhen (!do_rx && !(ready_to_tx)) {
      do_tx := False
      when(delay(4)) {
        do_rx := True
        delay := 0
      }
    } otherwise {
      delay := 0
    }
  }

  val pktend = new Area {
    val autoArmed = Reg(Bool) init (False)
    val manualArmed = Reg(Bool) init (False)
    val timeout = Reg(UInt(16 bits)) init (0)
    val pktendRequest = History(io.tx.pktend, 2)
    reg.fx3.pktend_n := True
    io.tx.pktend_done := False
    // reset if we write, still have to write or as long as we are full
    // start pktend auto timer when sending data
    when(!reg.fx3.wr_n || tx.needs_retransmit || !io.fx3.full_n) {
      when(!reg.fx3.wr_n) { autoArmed := True }
      when(pktendRequest(0) && !pktendRequest(1)) { manualArmed := True }
      timeout := 0
    } elsewhen (manualArmed || (autoArmed && (io.tx.pktend_timeout =/= 0) && (timeout === io.tx.pktend_timeout))) {
      autoArmed := False
      manualArmed := False
      timeout := 0
      reg.fx3.pktend_n := False
      io.tx.pktend_done := True
    } otherwise {
      when(pktendRequest(0) && !pktendRequest(1)) { manualArmed := True }
      timeout := timeout + 1
    }
  }
}
