package andreasWallner.spinaltap

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.bus.amba3.apb._
import spinal.lib.fsm._

case class UartModule() extends Component {
  val g = UartCtrlGenerics()

  val io = new Bundle {
    val uart = master(Uart())
    val apb = slave(Apb3(Apb3UartCtrl.getApb3Config))
    val events = master(Stream(Bits(16 bit)))
    val data = slave(Flow(Bits(16 bit)))

    val clockDivider = in UInt(g.clockDividerWidth bits)
  }

  val tx = new UartCtrlTx(g)
  val rx = new UartCtrlRx(g)

  val clockDivider = new Area {
    val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := io.clockDivider
    }
  }

  io.apb.PREADY := False
  io.apb.PRDATA := 0

  tx.io.samplingTick := clockDivider.tick
  rx.io.samplingTick := clockDivider.tick

  val frameConfig = UartCtrlFrameConfig(g)
  frameConfig.dataLength := 7
  frameConfig.parity := UartParityType.NONE
  frameConfig.stop := UartStopType.ONE
  tx.io.configFrame := frameConfig
  rx.io.configFrame := frameConfig

  val txFifo = StreamFifo(dataType=Bits(8 bit), depth=64)
  // queueWithAvailability? toStream on Flow?
  val rxFifo = StreamFifo(dataType=Bits(8 bit), depth=8)
  
  txFifo.io.push.payload := io.data.payload(0 until 8)
  txFifo.io.push.valid := io.data.valid
  txFifo.io.pop >> tx.io.write

  rx.io.read >> rxFifo.io.push

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd
  tx.io.cts := False
  tx.io.break := False
  
  val txOverflow = Reg(Bool).setWhen(txFifo.io.push.isStall)
  val txEmpty = txFifo.io.occupancy === 0
  val rxError = Reg(Bool).setWhen(rx.io.error)
  
  val statusHistory = History(txEmpty ## txOverflow ## rxError ## rxFifo.io.pop.valid, 2)
  val statusChange = Reg(Bool).setWhen(statusHistory(0) =/= statusHistory(1))

  val eventStateMachine = new StateMachine {
    val stateIdle = new State with EntryPoint
    val stateLen = new State
    val stateData = new State

    stateIdle
      .whenIsActive {
        when(rxFifo.io.pop.valid || statusChange) { goto(stateLen) }
      }
    
    io.events.payload := B"x0".resized
    io.events.valid := False
    stateLen
      .whenIsActive {
        io.events.payload := B"x1".resized
        io.events.valid := True
        when(io.events.ready) { goto(stateData) }
      }
    
      rxFifo.io.pop.ready := False
      stateData
        .whenIsActive {
          io.events.payload := (statusHistory(0) ## rxFifo.io.pop.payload).resized
          io.events.valid := True
          when(io.events.ready) { goto(stateIdle) }
        }
        .onExit(rxFifo.io.pop.ready := True, statusChange.clear())
  }
}