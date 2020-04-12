package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class LsiHostInterface() extends Bundle with IMasterSlave {
  val clock = Bool
  val data = TriState(Bool)
  val stop = Bool

  override def asMaster() {
    out(clock, stop)
    master(data)
  }

  override def asSlave() {
    in(clock, stop)
    master(data)
  }
}

case class HostWriteInterface() extends Bundle with IMasterSlave {
  val address = UInt(8 bits)
  val data = UInt(32 bits)
  val valid = Bool

  def asMaster() {
    out(address, data, valid)
  }
}

// TODO: valid signal to find timeout situations?
case class HostReadInterface() extends Bundle with IMasterSlave {
  val address = UInt(8 bits)
  val data = UInt(32 bits)
  val strb = Bool

  def asMaster() {
    out(address, strb)
    in(data)
  }
}

case class LsiMemInterface() extends Bundle with IMasterSlave {
  val hw = HostWriteInterface()
  val hr = HostReadInterface()

  def asMaster() {
    master(hw, hr)
  }
}

object Direction extends SpinalEnum {
  val Read, Write = newElement()
}

case class LsiInterface() extends Component {
  val io = new Bundle {
    val lsi = slave(LsiHostInterface())
    val mem = master(LsiMemInterface())
  }

  val sync = new Area {
    val clock = BufferCC(io.lsi.clock)
    val data = BufferCC(io.lsi.data.read)
    val stop = BufferCC(io.lsi.stop)
  }

  val clock_del = History(sync.clock, 2)
  val clock_edge = clock_del(0) ^ clock_del(1)

  val dataBuffer = Reg(Bits(32 + 8 bits))
  io.mem.hr.address := Reverse(dataBuffer(7 downto 0)).asUInt
  io.mem.hw.address := Reverse(dataBuffer(39 downto 32)).asUInt
  io.mem.hw.data := Reverse(dataBuffer(31 downto 0)).asUInt
  io.lsi.data.write := dataBuffer(39)

  io.mem.hr.strb := False

  val isRead = RegInit(False)
  val wasRead = RegNextWhen(isRead, clock_edge)

  io.lsi.data.writeEnable := isRead
  io.mem.hw.valid := False

  when (clock_edge) {
    switch(isRead) {
      is(True) {
        dataBuffer := dataBuffer(38 downto 0) ## False
        isRead := sync.stop
      }
      is(False) {
        isRead := sync.stop && sync.data
        io.mem.hw.valid := sync.stop && !sync.data
        io.mem.hr.strb := sync.stop && sync.data
        when(!sync.stop) {
          dataBuffer := dataBuffer(38 downto 0) ## sync.data
        }
      }
    }
  } otherwise {
    when(isRead && !wasRead) {
      dataBuffer(39 downto 8) := Reverse(io.mem.hr.data).asBits
    }
  }
}
