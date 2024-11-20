package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class DmaHandshake(hasBurst: Boolean = false) extends Bundle with IMasterSlave {
  val req = hasBurst generate Bool()
  val sreq = Bool()
  val waiting = Bool()
  val active = Bool()
  val done = Bool()

  val stall = Bool()
  val err = Bool()

  override def asMaster(): Unit = {
    out(active, done, err)
    in(req, sreq, stall)
  }
}

case class ChannelData(addressWidth: BitCount) extends Bundle {
  val active = Bool()
  val alternate = Bool()
  val src = TransferSettings(addressWidth)
  val dst = TransferSettings(addressWidth)
  val continue = Bool()
  val remaining = Reg(UInt(12 bit))

  def needsUpdate = remaining === 0 && continue
}

case class TransferSettings(addressWidth: BitCount) extends Bundle {
  val inc = Bool()
  val size = Bits(3 bit)
  val hprot = Bits(2 bit)
  val end = UInt(addressWidth)

  def address(remaining: UInt) = when(inc) { end - remaining } otherwise { end }
}

case class RegisterFile(managerConfig: AhbLite3Config) extends Area {
  val master_enable = Reg(Bool()) init False
  val channel_enable = Reg(Bool()) init False
  val config_address = Reg(UInt(managerConfig.addressWidth bit))
}

case class MicroDma(channelCnt: Int, slaveConfig: AhbLite3Config, managerConfig: AhbLite3Config) extends Component {
  val io = new Bundle {
    val sub = slave port AhbLite3(slaveConfig)
    val manager = master port AhbLite3(managerConfig)
    val x = Stream(Bool())

    val handshake = master port Vec(DmaHandshake(), channelCnt)
  }

  val regfile = RegisterFile(managerConfig)

  val channels = Vec(Reg(ChannelData(managerConfig.addressWidth bit)), channelCnt)
  val dataBuffer = Reg(Bits(managerConfig.dataWidth bit))

  val configFetch = new Area {
    val active = channels.map(c => c.active).asBits()
    val fetch_needed = regfile.master_enable && active.orR

  }
}
