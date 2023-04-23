package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.fsm._
import andreasWallner.spinaltap.Event

import scala.language.postfixOps

/** Decodes commands from PC, controls bus, routes events
 *
 * ==Supported Commands==
 *  - NOP
 *  - read register
 *  - write register
 *  - flush
 *
 * ===NOP (0x00)===
 * Added to have a way to get USB FIFOs working correctly in the
 * beginning where the FX3 SW/HW/Connection may swallow the first
 * frame.
 *
 * Example:
 * {{{
 * >> 0000
 * }}}
 *
 * ===Write Register (0x01)===
 * Example:
 * {{{
 * >> 01ss aaaa dddd dddd
 * <<
 *
 * s: 8bit source
 * a: 16bit address
 * d: 32bit data
 * }}}
 *
 * ===Read Register (0x02)===
 * Example:
 * {{{
 * >> 02ss aaaa
 * << 02ss dddd dddd
 *
 * s: 8bit source
 * a: 16bit address
 * d: 32bit data
 * }}}
 *
 * ===Flush (0x7F)===
 * Example:
 * {{{
 * >> 7frr
 * <<
 *
 * r: 8bit RFU
 * }}}
 *
 * ===Event (0x80)===
 * Any Opcode with MSB set is an async event:
 * {{{
 * << 80ss dddd
 *
 * s: 8bit source
 * d: 16bit event data
 * }}}
 */
case class BusMaster() extends Component {
  val io = new Bundle {
    val data = slave port Stream(Bits(16 bit))
    val resp = master port Stream(Bits(16 bit))
    val apb3 = master port Apb3(Apb3Config(16, 32))
    val events = slave port Stream(Event())
    val pktend = out port Bool()
    val pktend_done = in port Bool()
  }

  val fsm = new StateMachine {
    val stateIdle = new State with EntryPoint
    val stateAddress = new State
    val stateData0 = new State
    val stateData1 = new State
    val stateEnable = new State
    val stateWaitForReady = new State
    val stateSendOpcode = new State
    val stateSendData0 = new State
    val stateSendData1 = new State
    val stateStartFlush = new State
    val stateWaitFlush = new State
    val stateEventOpcode = new State
    val stateEventData = new State

    val opcode = Reg(Bits(8 bit))
    val write = Reg(Bool())
    val source = Reg(Bits(8 bit))
    val address = Reg(UInt(16 bit))
    val data = Reg(Bits(32 bit))
    val enable = Reg(Bool()) init False
    val sel = Reg(Bits(1 bit))

    io.apb3.PSEL := sel
    io.apb3.PADDR := address
    io.apb3.PWDATA := data
    io.apb3.PWRITE := write
    io.apb3.PENABLE := enable

    io.data.ready := False
    stateIdle
      .whenIsActive {
        when(io.data.valid) {
          io.data.ready := True
          opcode := io.data.payload(8 to 15)
          source := io.data.payload(0 to 7)

          switch(io.data.payload(8 to 15)) {
            is(B"x01") {
              write := True
              goto(stateAddress)
            }
            is(B"x02") {
              write := False
              goto(stateAddress)
            }
            is(B"x7F") {
              goto(stateStartFlush)
            }
          }
        } .elsewhen(io.events.valid) {
          goto(stateEventOpcode)
        }
      }

    stateAddress
      .whenIsActive {
        when(io.data.valid) {
          io.data.ready := True
          address := io.data.payload(0 to 15).asUInt
          when(write) { goto(stateData0) }
            .otherwise {
              sel := 1
              goto(stateEnable)
            }
        }
      }

    stateData0
      .whenIsActive {
        when(io.data.valid) {
          io.data.ready := True
          data(16 to 31) := io.data.payload
          goto(stateData1)
        }
      }

    stateData1
      .whenIsActive {
        when(io.data.valid) {
          io.data.ready := True
          data(0 to 15) := io.data.payload

          sel := 1

          goto(stateEnable)
        }
      }

    stateEnable
      .whenIsActive {
        enable := True
        goto(stateWaitForReady)
      }

    stateWaitForReady
      .whenIsActive {
        when(io.apb3.PREADY) {
          data := io.apb3.PRDATA
          when(write) { goto(stateIdle) }
            .otherwise { goto(stateSendOpcode) }
        }
      }
      .onExit {
        enable := False
        sel := 0
      }

    io.resp.payload := 0
    io.resp.valid := False
    stateSendOpcode
      .whenIsActive {
        io.resp.payload := opcode ## source
        io.resp.valid := True
        when(io.resp.ready) { goto(stateSendData0) }
      }

    stateSendData0
      .whenIsActive {
        io.resp.payload := data(16 to 31)
        io.resp.valid := True
        when(io.resp.ready) { goto(stateSendData1) }
      }

    stateSendData1
      .whenIsActive {
        io.resp.payload := data(0 to 15)
        io.resp.valid := True
        when(io.resp.ready) { goto(stateIdle) }
      }

    io.pktend := False
    stateStartFlush
      .whenIsActive {
        io.pktend := True
        goto(stateWaitFlush)
      }

    stateWaitFlush
      .whenIsActive {
        when(io.pktend_done) { goto(stateIdle) }
      }

    io.events.ready := False
    stateEventOpcode
      .onEntry(data(0 to 15) := io.events.payload.data)
      .whenIsActive {
        io.resp.payload := B"x80" ## io.events.payload.source
        io.resp.valid := True
        io.events.ready := True // master may not remove valid -> single cycle sufficient
        when(io.resp.ready) { goto(stateEventData) }
      }

    stateEventData
      .whenIsActive {
        io.resp.payload := data(0 to 15)
        io.resp.valid := True
        when(io.resp.ready) { goto(stateIdle) }
      }
  }
}
