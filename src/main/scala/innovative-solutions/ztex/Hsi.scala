package innovative_solutions.ztex

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

case class HsiInterface() extends Component {
  val io = new Bundle {
    val fx3 = master(FX3())
    val tx = new Bundle {
      val data = slave(Stream(Bits(16 bit)))
      val en = in Bool
      val pktend = in Bool
      val pktend_timeout = in UInt(16 bits)
    }
    val rx = new Bundle {
      val data = master(Stream(Bits(16 bit)))
    }
  }

  val do_rx = RegNext(True) init(False)

  val reg = new Area {
    val wr = Reg(False)
    val rd = RegNext(do_rx && !io.fx3.empty_n && io.rx.data.ready) init(False)
    val oe = Reg(False) init(False)

    io.fx3.wr_n := !wr
    io.fx3.rd_n := !rd
    io.fx3.oe_n := !oe
  }
  reg.wr := False

  io.fx3.dq.write := 0
  io.fx3.dq.writeEnable := False
  io.fx3.pktend_n := False
  io.tx.data.ready := False

  val rds = History(reg.rd, 3)
  val rx_buffer = new Area {
    val data = Vec(Reg(Bits(16 bit)), 3)
    val valid = Vec(Reg(Bool), 3)
  }
  io.rx.data.valid := False
  io.rx.data.payload := 0
  reg.oe := do_rx
  when(do_rx && io.fx3.empty_n && rds(2)) {
    when(io.rx.data.ready) {
      when(!rx_buffer.valid(0)) {
        io.rx.data.payload := io.fx3.dq.read
        io.rx.data.valid := True
      } otherwise {
        io.rx.data.payload := rx_buffer.data(0)
        rx_buffer.data(2) := 0
        rx_buffer.data(1) := rx_buffer.data(2)
        rx_buffer.data(0) := rx_buffer.data(1)
        rx_buffer.valid(2) := False
        rx_buffer.valid(1) := rx_buffer.valid(2)
        rx_buffer.valid(0) := rx_buffer.valid(1)
      }
    } otherwise {
      // backpressure but stream does not accept
      when(!rx_buffer.valid(0)) {
        rx_buffer.data(0) := io.fx3.dq.read
        rx_buffer.valid(0) := True
      } elsewhen (!rx_buffer.valid(1)) {
        rx_buffer.data(1) := io.fx3.dq.read
        rx_buffer.valid(1) := True
      } otherwise {
        rx_buffer.data(1) := io.fx3.dq.read
        rx_buffer.valid(1) := True
      }
    }
  }
}
