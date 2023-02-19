package andreasWallner.intro

import andreasWallner.Utils.divCeil
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.com.uart.{Uart, UartCtrl, UartCtrlInitConfig, UartParityType, UartStopType}
import spinal.lib.fsm._

import scala.language.postfixOps

object Action extends SpinalEnum {
  val NOOP, READ, WRITE, RESET = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    NOOP -> 0x0,
    READ -> 0x1,
    WRITE -> 0x2,
    RESET -> 0x7
  )
}

case class UartApbBridgeGenerics(
    apbConfig: Apb3Config,
    baudrate: Int = 115200,
    irqBits: BitCount = 0 bit
) {
  private def bytes(i: Int): Int = divCeil(i, 8)
  private val plainAddressBytes = bytes(apbConfig.addressWidth)

  val actionBits = 3 // get from action
  val addressBitsInAction = {
    val candidate = apbConfig.addressWidth - (plainAddressBytes - 1) * 8
    if (candidate > (8 - actionBits)) 0 else candidate
  }
  val addressBytes = bytes(apbConfig.addressWidth - addressBitsInAction)
  val dataBytes = bytes(apbConfig.dataWidth)

  val responseBytes = bytes(apbConfig.dataWidth) // TODO: header, etc.
}

case class UartApbBridge(g: UartApbBridgeGenerics) extends Module {
  assert(g.apbConfig.selWidth == 1, "only one sel bit supported")
  val io = new Bundle {
    val uart = master port Uart()
    val apb = master port Apb3(g.apbConfig)
    val irqs = in port Bits(g.irqBits)
    val doRst = out port Bool()
  }

  val uart = UartCtrl(
    config = UartCtrlInitConfig(
      baudrate = g.baudrate,
      dataLength = 7, // 8 bits
      parity = UartParityType.ODD,
      stop = UartStopType.TWO
    )
  ).io

  io.uart <> uart.uart

  val action = Reg(Bits(g.actionBits bit))
  val slicedAction = uart.read.payload.takeHigh(g.actionBits) // TODO .as[Action.E]

  io.apb.PSEL(0) := False
  io.apb.PADDR.setAsReg()
  io.apb.PWDATA.setAsReg()
  io.apb.PENABLE.setAsReg() init False
  io.apb.PWRITE := action === Action.WRITE.asBits
  val pwdataBytes = io.apb.PWDATA.subdivideIn(8 bit).reverse

  uart.read.ready := True // TODO - should always be fast enough...
  uart.write.payload.clearAll()
  uart.write.valid := False

  io.doRst := False

  //noinspection ForwardReference
  val fsm = new StateMachine {
    always {
      when(uart.readBreak || uart.readError) {
        forceGoto(idle)
      }
    }

    val idle = new State() with EntryPoint {
      whenIsActive {
        when(uart.read.valid) {
          action := slicedAction
          when((slicedAction === Action.READ.asBits) || (slicedAction === Action.WRITE.asBits)) {
            if (g.addressBitsInAction != 0) {
              val paddrSlice = io.apb.PADDR.high downto (io.apb.PADDR.high - g.addressBitsInAction + 1)
              io.apb.PADDR(paddrSlice) := uart.read.payload
                .takeLow(g.addressBitsInAction)
                .asUInt
            }

            if (g.addressBytes > 0) {
              goto(rxAddress)
            } else {
              when(slicedAction === Action.READ.asBits) {
                goto(doRead)
              } otherwise {
                goto(rxData)
              }
            }
          } elsewhen (slicedAction === Action.RESET.asBits) {
            goto(doReset)
          }
        }
      }
    }

    val addressByte = Reg(UInt(log2Up(g.addressBytes) + 1 bit))
    val rxAddress: State = if (g.addressBytes > 0) new State() {
      onEntry(addressByte := 0)
      whenIsActive {
        when(uart.read.valid) {
          switch(addressByte) {
            for (i <- g.addressBytes - 1 downto 0) yield is(i) {
              io.apb.PADDR(i * 8 + 7 downto i * 8).assignFromBits(uart.read.payload)
            }
          }

          addressByte := addressByte + 1
          when(addressByte === g.addressBytes - 1) {
            when(action === Action.READ.asBits) {
              goto(doRead)
            } otherwise {
              goto(rxData)
            }
          }
        }
      }
    }
    else null

    val dataByte = Reg(UInt(log2Up(g.dataBytes) bit))
    val rxData: State = new State() {
      onEntry(dataByte := 0)
      whenIsActive {
        when(uart.read.valid) {
          switch(dataByte) {
            for (i <- g.dataBytes - 1 downto 0) yield is(i) { pwdataBytes(i) := uart.read.payload }
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
      whenIsActive {
        // PENABLE is registered, driving off of PSEL makes it go high one cycle later
        io.apb.PSEL(0) := True
        io.apb.PENABLE := io.apb.PSEL(0)

        when(io.apb.PENABLE && io.apb.PREADY) {
          sendBytes := g.dataBytes
          sendBuffer := Cat(io.apb.PRDATA.subdivideIn(8 bit).reverse)
          goto(txResponse)
        }
      }
      onExit(io.apb.PENABLE := False)
    }
    val doWrite: State = new State() {
      whenIsActive {
        io.apb.PSEL(0) := True
        io.apb.PENABLE := io.apb.PSEL(0)

        when(io.apb.PENABLE && io.apb.PREADY) {
          sendBytes := 0
          goto(txResponse)
        }
      }
      onExit(io.apb.PENABLE := False)
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
      onEntry(sentBytes := 0)
      whenIsActive {
        uart.write.valid := True
        uart.write.payload := sentBytes.muxListDc(
          List(0 -> action ## B"00000") ++
            (for (i <- 0 until g.responseBytes) yield i + 1 -> sendBufferBytes(i))
        )
        when(uart.write.fire) {
          sentBytes := sentBytes + 1
          when(sentBytes === sendBytes) {
            goto(idle)
          }
        }
      }
    }
  }
}
