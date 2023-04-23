package andreasWallner.io.i3c

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.lib.fsm._

import scala.language.postfixOps

// TODO check that we use push-pull / opendrain in the correct places
// TODO allow for 0 size reads/writes

case class I3C(slaveOnly: Boolean = false) extends Bundle with IMasterSlave {
  val scl, sda = TriState(Bool())
  val pullup = if (!slaveOnly) Some(Bool()) else None
  val keeper = if (!slaveOnly) Some(Bool()) else None

  def asMaster(): Unit = {
    master(scl, sda)
    pullup.map(out.applyIt)
    keeper.map(out.applyIt)
  }
}
// TODO enforce time between stop & start
case class SdaTx() extends Component {
  val io = new Bundle {
    val data = slave port Stream(Fragment(Bits(8 bit)))
    val rxData = master port Flow(Bits(8 bit))
    val trigger = in port Bool()
    val continueRx = in port Bool()

    val i3c = master port I3C()

    val ack = master port Flow(Bool())
    val useRestart = in port Bool()

    val tCas = in port UInt(5 bit) // 38.4 ns < tCas < 50 mill (lowest activity state) Table 74
    val tCbp = in port UInt(5 bit)
    val changeCnt = in port UInt(5 bit) // pretty much implements tsco
    val lowCnt = in port UInt(5 bit) // take up low time bitCnt - lowCnt > 24 ns
    val bitCnt = in port UInt(5 bit) // remainder of bit for high pulse
    val stopCnt = in port UInt(5 bit)

    val idle = out port Bool()
    val rStart = out port Bool()
  }
  val timing = new Area {
    private val cnt = Reg(UInt(5 bit)) init 0
    cnt := cnt + 1
    def reset(): Unit = { cnt := 0 }

    val cas = cnt === io.tCas
    val change = cnt === io.changeCnt
    val clock = cnt === io.lowCnt
    val bit = cnt === io.bitCnt
    val stop = cnt === io.stopCnt
  }
  val sendData = new Area {
    private val bits = Reg(Bits(9 bit))
    val lastByte = Reg(Bool())
    val sendParity = Reg(Bool())
    val paritySent = Reg(Bool())
    val parityBit = Reg(Bool())

    val allSent = bits === B"000000001"

    io.data.ready := False
    def load(withParity: Bool): Unit = {
      lastByte := io.data.last
      bits := True ## io.data.fragment.reversed
      io.data.ready := True

      sendParity := withParity
      paritySent := False
      parityBit := True
    }
    def nextBit() = {
      bits := bits |>> 1
      parityBit := parityBit ^ bits(0)
      bits(0)
    }
  }
  val receiveData = new Area {
    private val bits = Reg(Bits(9 bit))
    val byteComplete = bits(8)

    io.rxData.payload := bits(7 downto 0) // TODO do we want a register here? (to keep data constant until next)
    io.rxData.valid := False

    def store(bit: Bool): Unit = {
      bits := bits(7 downto 0) ## bit
    }
    def reset(): Unit = {
      bits := B"000000001"
    }
    def pushData(): Unit = {
      io.rxData.valid := True
    }
  }

  io.i3c.pullup.foreach(_ := True)
  io.i3c.keeper.foreach(_ := False)
  io.i3c.scl.writeEnable.setAsReg init False
  io.i3c.sda.writeEnable.setAsReg init False
  io.i3c.scl.write.setAsReg
  io.i3c.sda.write.setAsReg

  io.ack.payload.setAsReg
  io.ack.valid := False

  io.idle := False
  io.rStart := False

  val isRead = Reg(Bool())
  val isAddress = Reg(Bool())
  //noinspection ForwardReference
  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        timing.reset()
        io.i3c.scl.writeEnable := False
        io.i3c.sda.writeEnable := False
        io.i3c.scl.write := True
        io.i3c.sda.write := True
        io.idle := True

        when(io.trigger) {
          sendData.load(False)
          isRead := io.data.payload.fragment(0)
          isAddress := True
          goto(start)
        }
      }
    }
    val start: State = new State {
      onEntry {
        io.i3c.sda.writeEnable := True
        io.i3c.scl.writeEnable := True
        io.i3c.sda.write := False
        io.i3c.scl.write := True
      }
      whenIsActive {
        io.i3c.sda.write := False
        when(timing.cas) { goto(dataLow) }
      }
      onExit {
        io.i3c.scl.write := False
        timing.reset()
      }
    }
    val rStart: State = new State {
      onEntry { timing.reset() }
      whenIsActive {
        io.rStart := True
        when(timing.change) {
          io.i3c.sda.write := True
          io.i3c.sda.writeEnable := True
        }
        when(timing.clock) {
          io.i3c.scl.write := True
        }
        when(timing.stop) {
          sendData.load(False)
          isRead := io.data.payload.fragment(0)
          isAddress := True
          io.i3c.sda.write := False
          goto(rStart2) // split into two bec. timing.cas most likely happens before timing.stop
        }
      }
    }
    val rStart2: State = new State { // TODO can we reuse the start state here?
      onEntry { timing.reset() }
      whenIsActive {
        when(timing.cas) { goto(dataLow) }
      }
      onExit {
        io.i3c.scl.write := False
        timing.reset()
      }
    }

    // for now use hard coded setup time of 1 cycle (>= 3 ns)
    val dataLow: State = new State {
      onEntry { io.i3c.sda.write := False }
      whenIsActive {
        when(timing.change) {
          when(!sendData.allSent) {
            io.i3c.sda.writeEnable := !sendData.nextBit()
          } otherwise {
            io.i3c.sda.writeEnable := !sendData.parityBit
            sendData.paritySent := True
          }
          goto(dataHigh)
        }
      }
    }
    val dataHigh: State = new State {
      whenIsActive {
        when(timing.clock) {
          io.i3c.scl.write := True
        }

        when(timing.bit) {
          io.i3c.scl.write := False
          when(sendData.allSent && !sendData.sendParity) {
            goto(ack)
          } elsewhen (sendData.allSent && sendData.sendParity && sendData.paritySent) {
            when(!sendData.lastByte) {
              sendData.load(True)
              goto(dataLow)
            } otherwise {
              when(!io.useRestart) {
                goto(stop)
              } otherwise {
                goto(rStart)
              }
            }
          } otherwise {
            goto(dataLow)
          }
        }
      }
      onExit(timing.reset())
    }

    val ackIsLow = !(isRead && !isAddress)
    val ack: State = new State {
      whenIsActive {
        when(timing.change) { io.i3c.sda.writeEnable := False }
        when(timing.clock) {
          io.i3c.scl.write := True
          io.ack.payload := io.i3c.sda.read ^ ackIsLow
          // if write continue driving ACK, overlapping with target
          when(!io.i3c.sda.read && (!isRead || (isRead && !isAddress))) {
            io.i3c.sda.write := False
            io.i3c.sda.writeEnable := True
          }
          when(isRead && !isAddress && io.i3c.sda.read && !io.continueRx) {
            goto(rxAbort)
          }
        }

        when(timing.bit) {
          isAddress := False
          io.i3c.scl.write := False
          io.ack.valid := True
          when(isRead && io.ack.payload) {
            goto(readBits)
          } elsewhen(!isRead && io.ack.payload && !sendData.lastByte) {
            sendData.load(True)
            goto(dataLow)
          } otherwise {
            // ack and we are out of data -> stop or repeated start
            // nack -> stop or repeated start
            // TODO separate restart input for NACK case?
            when(!io.useRestart) {
              goto(stop)
            } otherwise {
              goto(rStart)
            }
          }
        }
      }
      onExit(timing.reset())
    }

    val rxAbort: State = new State {
      onEntry { timing.reset() }
      whenIsActive {
        when(!io.i3c.sda.writeEnable && timing.cas) { // TODO should be Tcas/2 or external setting
          io.i3c.sda.write := False // TODO needed?
          io.i3c.sda.writeEnable := True
          timing.reset()
        }
        when(io.i3c.sda.writeEnable && timing.cas) { // TODO should be Tcas/2 or external setting
          io.i3c.scl.write := False
          when(io.useRestart) {
            isAddress := True
            sendData.load(False)
            isRead := io.data.payload.fragment(0)
            timing.reset()
            goto(dataLow)
          } otherwise {
            goto(stop)
          }
        }
      }
    }

    val readBits: State = new State {
      onEntry {
        timing.reset()
        receiveData.reset()
      }
      whenIsActive {
        when(timing.clock) {
          io.i3c.scl.write := True
          receiveData.store(io.i3c.sda.read)
        }
        when(timing.bit) {
          io.i3c.scl.write := False
          timing.reset()
          when(receiveData.byteComplete) {
            receiveData.pushData()
            receiveData.reset()
            goto(ack)
          }
        }
      }
    }

    val stop: State = new State {
      onEntry { timing.reset() }
      whenIsActive {
        when(timing.change) {
          io.i3c.sda.writeEnable := True
          io.i3c.sda.write := False
        }
        when(timing.clock) {
          io.i3c.scl.write := True
        }
        when(timing.stop) {
          io.i3c.sda.write := True
          // idle state takes care that writeEnable is disabled again
          goto(idle)
        }
      }
    }
  }
}

case class I3CController() extends Component {
  val io = new Bundle {
    val i3c = master(I3C())
  }
}
