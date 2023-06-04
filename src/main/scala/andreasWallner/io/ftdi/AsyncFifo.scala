package andreasWallner.io.ftdi

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState

import scala.language.postfixOps

case class AsyncFifo() extends Bundle with IMasterSlave {
  val d = TriState(Bits(8 bit))
  /** FTDI RX buffer has data */
  val rxf_n = Bool()
  /** FTDI TX buffer is not full */
  val txe_n = Bool()
  val rd_n = Bool()
  val wr_n = Bool()

  def asMaster(): Unit = {
    master(d)
    in(rxf_n, txe_n)
    out(rd_n, wr_n)
  }
}

case class AsyncFifoController () extends Component {
  val io = new Bundle {
    val fifo = master port AsyncFifo()
    val tx = slave port Stream(Bits(8 bit))
    val rx = master port Stream(Bits(8 bit))
  }
  def toCycles(tn: TimeNumber) = (tn * ClockDomain.current.frequency.getValue).toBigInt

  val txnf = !BufferCC(io.fifo.txe_n, init=True)
  val rxne = !BufferCC(io.fifo.rxf_n, init=True)

  io.fifo.d.write.setAsReg()
  io.fifo.d.writeEnable.setAsReg().init(False)
  io.fifo.wr_n.setAsReg().init(False)
  io.fifo.rd_n.setAsReg().init(True)
  io.tx.ready := False

  // have both valid & payload as a register:
  // if we RX we are not blocked from continuing with TX if there is no io.rx.ready
  // and we can't wait for ready is it may be deasserted any cycle
  io.rx.valid.setAsReg().init(False)
  io.rx.valid.clearWhen(io.rx.fire)
  io.rx.payload.setAsReg()

  // track cycles until we can trust TXE#/RXF# again
  val txPrecharge = Timeout(toCycles(80 ns) + 3)
  txPrecharge.init(True)
  val rxPrecharge = Timeout(toCycles((50 + 80) ns))
  rxPrecharge.init(True)

  //noinspection ForwardReference
  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.fifo.d.writeEnable := False
        when(txnf && io.tx.valid && txPrecharge) {
          goto(tx)
        }
        when(rxne && !io.rx.valid && rxPrecharge) {
          goto(rx)
        }
      }
    }

    val tx: State = new StateDelay(50 ns) {
      onEntry {
        io.fifo.d.write := io.tx.payload
        io.fifo.d.writeEnable := True
        io.fifo.wr_n := True
      }
      whenCompleted {
        txPrecharge.clear()
        io.tx.ready := True
        io.fifo.wr_n := False
        goto(idle)
      }
    }

    val rx: State = new StateDelay(toCycles(50 ns) + 3) {
      onEntry {
        io.fifo.rd_n := False
      }
      whenCompleted {
        rxPrecharge.clear()
        io.fifo.rd_n := True
        io.rx.valid := True
        io.rx.payload := io.fifo.d.read
        goto(idle)
      }
    }
  }
}
