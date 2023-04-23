package andreasWallner.spinaltap

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.bus.amba3.apb._
import spinal.lib.fsm._
import andreasWallner.spinaltap

import scala.language.postfixOps

case class UartModule(eventSourceId: Int) extends Component {
  val g = UartCtrlGenerics()

  val io = new Bundle {
    val uart = master port Uart()
    val bus = slave port Apb3(Apb3UartCtrl.getApb3Config)
    val events = master port Stream(spinaltap.Event())

    val clockDivider = in UInt(g.clockDividerWidth bits)
  }

  val busFactory = Apb3SlaveFactory(io.bus)
  val tx = new UartCtrlTx(g)
  val rx = new UartCtrlRx(g)

  val clockDivider = new Area {
    val counter = Reg(UInt(g.clockDividerWidth bits)) init 0
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := io.clockDivider
    }
  }

  tx.io.samplingTick := clockDivider.tick
  rx.io.samplingTick := clockDivider.tick

  val frameConfig = UartCtrlFrameConfig(g)
  frameConfig.dataLength := 7
  frameConfig.parity := UartParityType.NONE
  frameConfig.stop := UartStopType.ONE
  tx.io.configFrame := frameConfig
  rx.io.configFrame := frameConfig

  val txFifo = StreamFifo(dataType=Bits(8 bit), depth=64)
  val txUnbuffered = busFactory.createAndDriveFlow(Bits(g.dataWidthMax bits), 0x00, 0).toStream
  txFifo.io.push <> txUnbuffered
  txFifo.io.pop >> tx.io.write

  // queueWithAvailability? toStream on Flow?
  val rxFifo = StreamFifo(dataType=Bits(8 bit), depth=64)
  rx.io.read >> rxFifo.io.push

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd
  tx.io.cts := False
  tx.io.break := False

  val txOverflow = Reg(Bool()).setWhen(txFifo.io.push.isStall)
  val txEmpty = txFifo.io.occupancy === 0
  val rxError = Reg(Bool()).setWhen(rx.io.error)

  val statusHistory = History(txEmpty ## txOverflow ## rxError ## rxFifo.io.pop.valid, 2)
  val statusChange = Reg(Bool()).setWhen(statusHistory(0) =/= statusHistory(1))

  val eventStateMachine = new StateMachine {
    val stateIdle = new State with EntryPoint
    val stateLen = new State
    val stateData = new State

    // TODO no state machine needed anymore
    // TODO test that we do not skip a notification if state change happens in the cycle we are getting a ready
    stateIdle
      .whenIsActive {
        when(rxFifo.io.pop.valid || statusChange) { goto(stateData) }
      }

    io.events.payload.source := eventSourceId
    io.events.payload.data := 0
    io.events.valid := False
    rxFifo.io.pop.ready := False
    stateData
      .whenIsActive {
        io.events.payload.data := (statusHistory(0) ## rxFifo.io.pop.payload).resized
        io.events.valid := True
        when(io.events.ready) { goto(stateIdle) }
      }
      .onExit {
        rxFifo.io.pop.ready := True
        statusChange.clear()
      }
  }
}
