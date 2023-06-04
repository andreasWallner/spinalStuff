package andreasWallner.bus.remote

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import andreasWallner.Utils.divCeil
import spinal.lib.fsm._

import scala.language.postfixOps

object Action extends SpinalEnum {
  val NOOP, READ, WRITE, RESET = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    NOOP -> 0,
    READ -> 1,
    WRITE -> 2,
    RESET -> 7
  )
}

case class ApbMasterGenerics(apbConfig: Apb3Config, irqBits: BitCount = 0 bit) {
  private def bytes(i: Int): Int = divCeil(i, 8)
  private val plainAddressBytes = bytes(apbConfig.addressWidth)

  val actionBits = Action.NOOP.getBitsWidth
  val addressBitsInAction = {
    val candidate = apbConfig.addressWidth - (plainAddressBytes - 1) * 8
    if (candidate > (8 - actionBits)) 0 else candidate
  }
  val addressBytes = bytes(apbConfig.addressWidth - addressBitsInAction)
  val dataBytes = bytes(apbConfig.dataWidth)

  val responseBytes = bytes(apbConfig.dataWidth) // TODO: header, response
}

case class ApbMaster(g: ApbMasterGenerics) extends Module {
  assert(g.apbConfig.selWidth == 1, "only one sel bit is supported, use decoder if you need more")
  val io = new Bundle {
    val cmd = slave port Stream(Bits(8 bit))
    val resp = master port Stream(Bits(8 bit))
    val abort = slave port Event
    val irqs = in port Bits(g.irqBits)
    val doRst = out port Bool()

    val apb = master port Apb3(g.apbConfig)
  }

  val action = Reg(Action())
  val slicedAction = io.cmd.payload.takeHigh(action.getBitsWidth)
  val convAction = Action()
  convAction.assignFromBits(slicedAction)

  io.cmd.ready := True
  io.resp.payload.clearAll()
  io.resp.valid := False

  io.doRst := False
  io.abort.ready := True

  io.apb.PSEL(0) := False
  io.apb.PADDR.setAsReg()
  io.apb.PWDATA.setAsReg()
  io.apb.PENABLE.setAsReg() init False
  io.apb.PWRITE := action === Action.WRITE
  val pwdataBytes = io.apb.PWDATA.subdivideIn(8 bit).reverse

  //noinspection ForwardReference
  val fsm = new StateMachine {
    always {
      when(io.abort.valid) {
        forceGoto(idle)
      }
    }

    val idle = new State() with EntryPoint {
      whenIsActive {
        when(io.cmd.valid) {
          action := convAction
          when((convAction === Action.READ) || (convAction === Action.WRITE)) {
            if (g.addressBitsInAction != 0) {
              val paddrSlice = io.apb.PADDR.high downto (io.apb.PADDR.high - g.addressBitsInAction + 1)
              io.apb.PADDR(paddrSlice) := io.cmd.payload.takeLow(g.addressBitsInAction).asUInt
            }

            if(g.addressBytes > 0) {
              goto(rxAddress)
            } else {
              when(convAction === Action.READ) {
                goto(doRead)
              } otherwise {
                goto(rxData)
              }
            }
          } elsewhen(convAction === Action.RESET) {
            goto(doReset)
          }
        }
      }
    }

    val addressByte = Reg(UInt(log2Up(g.addressBytes) + 1 bit))
    val rxAddress: State = if(g.addressBytes > 0) new State() {
      onEntry { addressByte := 0}
      whenIsActive {
        when(io.cmd.valid) {
          switch(addressByte) {
            for(i <- g.addressBytes - 1 downto 0) yield is(i) {
              io.apb.PADDR(i*8 + 7 downto i * 8).assignFromBits(io.cmd.payload)
            }
          }
        }

        addressByte := addressByte + 1
        when(addressByte === g.addressBytes - 1) {
          when(action === Action.READ) {
            goto(doRead)
          } otherwise {
            goto(rxData)
          }
        }
      }
    } else null

    val dataByte = Reg(UInt(log2Up(g.dataBytes) bit))
    val rxData: State = new State() {
      onEntry{dataByte := 0}
      whenIsActive{
        when(io.cmd.valid) {
          switch(dataByte) {
            for(i <- g.dataBytes - 1 downto 0) yield is(i) { pwdataBytes(i) := io.cmd.payload }
          }
          dataByte := dataByte + 1
          when(dataByte === g.dataBytes - 1) {
            goto(doWrite)
          }
        }
      }
    }

    val sendBytes = Reg(UInt(3 bit))
    val sendBuffer = Reg(Bits(g.apbConfig.dataWidth bit))
    val doRead: State = new State() {
      whenIsActive{
        // PENABLE is registered, driving off of PSEL makes it go high one cycle later
        io.apb.PSEL(0) := True
        io.apb.PENABLE := io.apb.PSEL(0)

        when(io.apb.PENABLE && io.apb.PREADY) {
          sendBytes := g.dataBytes
          sendBuffer := Cat(io.apb.PRDATA.subdivideIn(8 bit).reverse)
          goto(txResponse)
        }
      }
      onExit { io.apb.PENABLE := False }
    }

    val doWrite: State = new State() {
      whenIsActive {
        // PENABLE is registered, driving off of PSEL makes it go high one cycle later
        io.apb.PSEL(0) := True
        io.apb.PENABLE := io.apb.PSEL(0)

        when(io.apb.PENABLE && io.apb.PREADY) {
          sendBytes := 0
          goto(txResponse)
        }
      }
      onExit{io.apb.PENABLE := False}
    }

    val doReset: State = new State() {
      whenIsActive {
        io.doRst := True
        sendBytes := 0
        goto(txResponse)
      }
    }

    val sendBufferBytes = sendBuffer.subdivideIn(8 bit)
    val sentBytes = Reg(UInt(2 bit))
    val txResponse: State = new State() {
      onEntry { sentBytes := 0 }
      whenIsActive {
        io.resp.valid := True
        io.resp.payload := sentBytes.muxListDc(
          List(0 -> action ## B"000000") ++ (for(i <- 0 until g.responseBytes) yield i+1 -> sendBufferBytes(i))
        )
        when(io.resp.fire) {
          sentBytes := sentBytes + 1
          when(sentBytes === sendBytes) {
            goto(idle)
          }
        }
      }
    }
  }
}
