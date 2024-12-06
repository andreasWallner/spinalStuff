package andreasWallner.bus.amba3.ahblite3

// where to put the multiplexers?
// use a single core for all channels:
//   - need a multiplexer for info & data fetching -> arbitration more complex, should it depend on channel prio?
//   - or: double channels and have one "peripheral" as data fetcher? (big?)
// one core per channel:
//   - big multiplexers for data ordering into buffer?
//   - initial bus aligner can be reused in any case
//   - no need to finish a burst to switch to another channel

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba3.ahblite._

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
    out(active, done, err, waiting)
    in(req, sreq, stall)
  }
}

/// Invariants
/// - active && remaining == 0 -> we have not yet fetched transfer settings
/// - active && remaining != 0 -> we are in the process of transferring data
/// - !active -> this channel is idle
///
/// With the last transfer that sets remaining to 0 we also clear active, and activate
/// the next channel in case continue is set.
/// The next channel is chosen depending on the mode.
case class ChannelData(addressWidth: BitCount) extends Bundle {
  val active = Bool()
  val alternate = Bool()
  val src = TransferSettings(addressWidth)
  val dst = TransferSettings(addressWidth)
  val continue = Bool()

  def needsInfoFetch = src.remaining === 0 && active

  def initIdle() = {
    active init False
    alternate init False
    continue init True
    src.remaining init 0
  }
}

case class TransferSettings(addressWidth: BitCount) extends Bundle {
  val inc = Bool()
  val size = Bits(3 bit)
  val hprot = Bits(4 bit)
  val end = UInt(addressWidth)
  val remaining = Reg(UInt(16 bit))

  def sizeBytes = {
    val result = UInt(8 bit) // TODO make output and following adders smaller
    result := size.mux(
      0 -> U(1),
      1 -> U(2),
      2 -> U(4),
      3 -> U(8),
      4 -> U(16),
      5 -> U(32),
      6 -> U(64),
      7 -> U(128)
    )
    result
  }
}

case class MicroDmaGenerics(
    channelCnt: Int,
    slaveConfig: AhbLite3Config,
    managerConfig: AhbLite3Config
) {
  def channelIdxWidth = log2Up(channelCnt)
  // alternate bit + channel index + config size + config element size
  def baseAddressOffset = 1 + channelIdxWidth + 2 + 2
  def baseAddressWidth = managerConfig.dataWidth - baseAddressOffset
}

case class RegisterFile(generics: MicroDmaGenerics, ahb: AhbLite3) extends Area {
  val masterEnable = Reg(Bool()) init False
  val channelEnable = Reg(Bits(generics.channelCnt bit)) init 0
  val configAddress = Reg(UInt(generics.baseAddressWidth bit))

  val factory = new AhbLite3SlaveFactory(ahb)
  factory.readAndWrite(masterEnable, 0x00, 0)
  factory.readAndWrite(channelEnable, 0x00, 1)
  factory.readAndWrite(configAddress, 0x04, generics.baseAddressOffset)
}

case class MicroDma(generics: MicroDmaGenerics) extends Component {
  val io = new Bundle {
    val sub = slave port AhbLite3(generics.slaveConfig)
    val manager = master port AhbLite3Master(generics.managerConfig)
    val handshake = Vec(master port DmaHandshake(), generics.channelCnt)
  }

  val regfile = RegisterFile(generics, io.sub)
  io.handshake.zipWithIndex.foreach{case (h, idx) => {
    h.waiting := False
    h.active := regfile.channelEnable(idx)
    h.done := False
    h.err := False
  }}

  val dataSlices = new Area {
    private val sel = RegNextWhen(io.manager.HADDR(1 downto 0), io.manager.HREADY)
    val data8 = io.manager.HRDATA.subdivideIn(8 bit)(sel)
    val data16 = io.manager.HRDATA.subdivideIn(16 bit)(sel >> 1)
    val data32 = io.manager.HRDATA
  }

  // what we are fetching
  // - src end  0x00
  // - dst end  0x04
  // - config   0x08
  // - padding
  //
  // Idea: since we fetch config (containing "remaining") last we don't need to keep track
  // of that state, just checking for remaining != 0 is sufficient -> less logic in that path
  // TODO: check if that really is the case, of if it's smaller to just use the small counter as a state... faster it should be
  //
  // so addresses are
  // 0bxxxxaccoo00
  //      || | \_ above offset
  //      || \___ channel idx, width depends on channel count
  //      |\_____ alternate bit
  //      \______ base address
  val masterPorts = Vec(AhbLite3Master(generics.managerConfig), generics.channelCnt)
  val channels = Vec(Reg(ChannelData(generics.managerConfig.addressWidth bit)), generics.channelCnt)
  for (idx <- 0 until generics.channelCnt) new Composite(channels(idx)) {
    val dataBuffer = Reg(Bits(generics.managerConfig.dataWidth bit))
    val channel = channels(idx)
    channel.initIdle()
    val port = masterPorts(idx)

    val addressPhase = Reg(UInt(2 bit))
    val infoComplete = Reg(Bool()) init False

    val infoAddress = regfile.configAddress @@
      channel.alternate @@
      U(idx, generics.channelIdxWidth bit) @@
      addressPhase @@
      U(0, 2 bit)
    port.HADDR := infoAddress
    port.HWRITE := False
    port.HSIZE := 2
    port.HBURST := 0
    port.HTRANS := channel.active ? AhbLite3.NONSEQ | AhbLite3.IDLE
    port.HMASTLOCK := False
    port.HPROT := 0

    val transferActive = Reg(Bool()) init False
    when(!infoComplete && transferActive && port.HREADY) {
      transferActive := False
      switch(addressPhase) {
        is(1) { channel.src.end := port.HRDATA.asUInt }
        is(2) { channel.dst.end := port.HRDATA.asUInt }
        is(3) {
          channel.src.inc := port.HRDATA(31)
          channel.dst.inc := port.HRDATA(30)
          channel.src.size := port.HRDATA(29 downto 27)
          channel.dst.size := port.HRDATA(26 downto 24)
          channel.src.hprot := port.HRDATA(23 downto 20)
          channel.dst.hprot := port.HRDATA(19 downto 16)
          channel.src.remaining := (port
            .HRDATA(15 downto 0)
            .asUInt << port.HRDATA(29 downto 27).asUInt).resized // TODO get rid of this
          channel.dst.remaining := (port
            .HRDATA(15 downto 0)
            .asUInt << port.HRDATA(26 downto 24).asUInt).resized
          infoComplete := True
        }
      }
    }
    when(port.HTRANS === AhbLite3.NONSEQ && port.HREADY && addressPhase =/= 3) {
      addressPhase := addressPhase + 1
      transferActive := True
    }

    // TODO read write bus error forwarding & abort
    val reading = Reg(Bool()) init True
    val writing = Reg(Bool()) init False
    val readingDP = RegNextWhen(reading, infoComplete && port.HREADY)
    val writingDP = RegNextWhen(writing, infoComplete && port.HREADY)
    val bufferFill = Reg(UInt(8 bit))
    val srcAddress = channel.src.end - channel.src.remaining
    val dstAddress = channel.dst.end - channel.dst.remaining
    val dstBytes = channel.dst.sizeBytes
    val srcBytes = channel.src.sizeBytes
    val fillAdd = bufferFill + srcBytes
    val fillSub = bufferFill - dstBytes
    val canTransmit = (bufferFill + srcBytes) > dstBytes
    when(infoComplete && channel.active) {
      port.HTRANS := (reading || writing) ? AhbLite3.NONSEQ | AhbLite3.IDLE
      port.HWRITE := writing
      port.HADDR := reading.mux(srcAddress, dstAddress)
      port.HSIZE := reading.mux(channel.src.size, channel.dst.size)
      port.HPROT := reading.mux(channel.src.hprot, channel.dst.hprot)

      when(port.HREADY) {
        transferActive := True
        when(reading) {
          channel.src.remaining := channel.src.remaining - channel.dst.sizeBytes
          bufferFill := fillAdd
          reading := fillAdd =/= dstBytes
          writing := fillAdd === dstBytes
        } otherwise {
          channel.dst.remaining := channel.dst.remaining - channel.dst.sizeBytes
          bufferFill := fillSub
          writing := fillSub =/= 0
          when(channel.src.remaining === 0) {
            regfile.channelEnable(idx) := False
            infoComplete := False
          } otherwise {
            reading := fillSub === 0
          }
        }
      }
    }

    when(transferActive) {
      when(readingDP) {
        switch(srcBytes) {
          is(1) {
            switch(bufferFill) {
              is(1) { dataBuffer(7 downto 0) := dataSlices.data8 }
              is(2) { dataBuffer(15 downto 8) := dataSlices.data8 }
              is(3) { dataBuffer(23 downto 16) := dataSlices.data8 }
              is(0) { dataBuffer(31 downto 24) := dataSlices.data8 }
            }
          }
          is(2) {
            switch(bufferFill) {
              is(2) { dataBuffer(15 downto 0) := dataSlices.data16 }
              is(0) { dataBuffer(31 downto 16) := dataSlices.data16 }
            }
          }
          is(4) {
            dataBuffer := dataSlices.data32
          }
        }
      }
    }

    val data8 = bufferFill.muxDc(
      4 -> dataBuffer(7 downto 0),
      3 -> dataBuffer(15 downto 8),
      2 -> dataBuffer(23 downto 16),
      1 -> dataBuffer(31 downto 24)
    )
    val data16 = bufferFill.muxDc(
      0 -> dataBuffer(15 downto 0),
      2 -> dataBuffer(31 downto 16)
    )
    val data32 = CombInit(dataBuffer)

    port.HWDATA.assignDontCare()
    val sel = RegNextWhen(io.manager.HADDR(1 downto 0), io.manager.HREADY)
    switch(dstBytes) {
      is(1) {
        switch(sel) {
          is(0) { port.HWDATA(7 downto 0) := data8 }
          is(1) { port.HWDATA(15 downto 8) := data8 }
          is(2) { port.HWDATA(23 downto 16) := data8 }
          is(3) { port.HWDATA(31 downto 24) := data8 }
        }
      }
      is(2) {
        switch(sel(1 downto 1)) {
          is(0) { port.HWDATA(15 downto 0) := data16 }
          is(1) { port.HWDATA(31 downto 16) := data16 }
        }
      }
      is(3) {
        port.HWDATA := data32
      }
    }

    when(!channel.active) {
      addressPhase := 0
      bufferFill := 0
      channel.alternate := False
      reading := True
      writing := False
    }
  }

  //channels.zip(regfile.channelEnable.asBools).foreach(x => x._2 := x._1.active)
  channels(0).active := regfile.channelEnable(0)

  io.manager <> masterPorts(0)
}
