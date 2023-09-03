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

// format: off
case class AsyncFifoTimings(             // #199   #511
    // RX
    rd_to_data: TimeNumber,              // t3     t3
    rd_to_rxf: TimeNumber,               // t1     t5
    rd_pulse: TimeNumber,                // t4     t1
    rd_pause: Option[TimeNumber],        // -      t2

    // TX
    data_to_wr_setup: TimeNumber,        // t8     t9
    data_to_wr_hold: Option[TimeNumber], // t9     t10
    wr_pulse: TimeNumber,                // t10    t8
    wr_pause: Option[TimeNumber],        // -      t7

    // require that we have a delay of at least read data delay + 1 cycle
    // may be disabled for fast FIFO chip iff the delay is set up for timing checks
    require_rd_delay: Boolean
)
// format: on

object AsyncFifoController {
  def apply(io: AsyncFifo, timings: AsyncFifoTimings): AsyncFifoController = {
    val controller = AsyncFifoController(timings)
    controller.io.fifo <> io
    controller
  }

  def toCycles(tn: TimeNumber, correction: Int = 0) =
    ((tn * ClockDomain.current.frequency.getValue)
      .setScale(0, BigDecimal.RoundingMode.CEILING)
      .toBigInt - correction) max 0

  object Timings {
    // taken from FT232H spec, #199, v2.0
    //   modification: #199 states 5 ns of hold time for TX, but the later released #511
    //   with generally higher margins (and including FT232H) does not, so we skip it
    val FT232H = AsyncFifoTimings(14 ns, 14 ns, 30 ns, None, 5 ns, None, 30 ns, None, true)
    // taken from FIFO Basics, #511, v1.0
    //   covering FT245B, FT245R, FT240X, FT2232D, FT232H, FT2232H
    val safe =
      AsyncFifoTimings(50 ns, 25 ns, 50 ns, Some(130 ns), 20 ns, None, 50 ns, Some(50 ns), true)
  }
}

case class AsyncFifoController(timings: AsyncFifoTimings) extends Component {
  val io = new Bundle {
    val fifo = master port AsyncFifo()
    val tx = slave port Stream(Bits(8 bit))
    val rx = master port Stream(Bits(8 bit))
    val s = out port Bits(2 bit)
  }
  import AsyncFifoController.toCycles

  val txnf = !BufferCC(io.fifo.txe_n, init = True)
  val rxne = !BufferCC(io.fifo.rxf_n, init = True)

  io.fifo.d.write.setAsReg()
  io.fifo.d.writeEnable.setAsReg().init(False)
  io.fifo.wr_n.setAsReg().init(True)
  io.fifo.rd_n.setAsReg().init(True)
  io.tx.ready := False

  // have both valid & payload as a register:
  // if we RX we are not blocked from continuing with TX if there is no io.rx.ready
  // and we can't wait for ready is it may be deasserted any cycle
  io.rx.valid.setAsReg().init(False)
  io.rx.valid.clearWhen(io.rx.fire)
  io.rx.payload.setAsReg()

  // track cycles until we can trust TXE#/RXF# again
  val txPrecharge = timings.wr_pause.map(t => Timeout(toCycles(t)))
  //txPrecharge.init(True) // TODO
  val rxPrecharge = timings.rd_pause.map(t => Timeout(toCycles(t)))
  //rxPrecharge.init(True)
  io.s := 0
  //noinspection ForwardReference
  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.s := 1
        io.fifo.d.writeEnable := False
        when(txnf && io.tx.valid && txPrecharge.map(_.implicitValue).getOrElse(True)) {
          goto(tx)
        }
        when(rxne && !io.rx.valid && rxPrecharge.map(_.implicitValue).getOrElse(True)) { // or io.rx.ready
          goto(rx)
        }
      }
    }

    val tx: StateDelay = new StateDelay(toCycles(timings.wr_pulse)) {
      onEntry {
        io.fifo.d.write := io.tx.payload
        // writeEnable is disabled in idle state to make sure that we adhere to hold
        // TODO detail if necessary, remove parameter
        io.fifo.d.writeEnable := True
        io.fifo.wr_n := False
      }
      whenCompleted {
        txPrecharge.foreach(_.clear())
        io.tx.ready := True
        io.fifo.wr_n := True
        goto(idle)
      }
      whenIsActive { io.s := 2 }
    }

    val rx_time = toCycles(timings.rd_pulse) max (if (timings.require_rd_delay)
                                                    toCycles(timings.rd_to_data) + 1
                                                  else 0)
    val rx: StateDelay = new StateDelay(rx_time) {
      onEntry {
        io.fifo.rd_n := False
      }
      whenIsActive { io.s := 3 }
      whenCompleted {
        rxPrecharge.foreach(_.clear())
        io.fifo.rd_n := True
        io.rx.valid := True
        io.rx.payload := io.fifo.d.read
        goto(idle)
      }
    }
  }
}
