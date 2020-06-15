package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.fsm._

case class BusMaster() extends Component {
  val io = new Bundle {
    val data = slave(Stream(Bits(16 bit)))
    val resp = master(Stream(Bits(16 bit)))
    val apb3 = master(Apb3(Apb3Config(16, 32)))
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

    val opcode = Reg(Bits(8 bit))
    val write = Reg(Bool)
    val source = Reg(Bits(8 bit))
    val address = Reg(UInt(16 bit))
    val data = Reg(Bits(32 bit))
    val enable = Reg(Bool) init (False)
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
          }
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
      .onExit(enable := False, sel := 0)

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
  }
}
