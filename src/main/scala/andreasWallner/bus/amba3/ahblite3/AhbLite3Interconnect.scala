package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._

import scala.language.postfixOps

object SkidBuffer {
  def apply[T <: Data](i: T, hold: Bool) = {
    val buffer = new SkidBuffer(i)
    buffer.io.i := i
    buffer.io.hold := hold
    buffer.io.o
  }
  def apply[T <: Data](payloadType: HardType[T]) = {
    new SkidBuffer(payloadType)
  }
}

class SkidBuffer[T <: Data](payloadType: HardType[T]) extends Component {
  val io = new Bundle {
    val i = in port payloadType()
    val o = out port payloadType()
    val hold = in port Bool()
  }

  val reg = RegNextWhen(io.i, !io.hold)
  io.o := io.hold ? reg | io.i
}

object AhbLite3Control {
  def apply(master: AhbLite3Master) = {
    val ctrl = new AhbLite3Control(master.config)
    ctrl.HADDR := master.HADDR
    ctrl.HWRITE := master.HWRITE
    ctrl.HSIZE := master.HSIZE
    ctrl.HBURST := master.HBURST
    ctrl.HPROT := master.HPROT
    ctrl.HTRANS := master.HTRANS
    ctrl.HMASTLOCK := master.HMASTLOCK
    ctrl
  }
}
case class AhbLite3Control(config: AhbLite3Config) extends Bundle {
  val HADDR = UInt(config.addressWidth bits)
  val HWRITE = Bool()
  val HSIZE = Bits(3 bits)
  val HBURST = Bits(3 bits)
  val HPROT = Bits(4 bits)
  val HTRANS = Bits(2 bits)
  val HMASTLOCK = Bool()

  def drive(ahb: AhbLite3): Unit = {
    ahb.HADDR := HADDR
    ahb.HWRITE := HWRITE
    ahb.HSIZE := HSIZE
    ahb.HBURST := HBURST
    ahb.HPROT := HPROT
    ahb.HTRANS := HTRANS
    ahb.HMASTLOCK := HMASTLOCK
  }

  def withoutOffset(mapping: SizeMapping, newConfig: AhbLite3Config = null) = {
    val ahb = new AhbLite3Control(Option(newConfig).getOrElse(config))
    ahb.HADDR := mapping.removeOffset(HADDR).resized
    ahb.HWRITE := HWRITE
    ahb.HSIZE := HSIZE
    ahb.HBURST := HBURST
    ahb.HPROT := HPROT
    ahb.HTRANS := HTRANS
    ahb.HMASTLOCK := HMASTLOCK
    ahb
  }
}

object AhbLite3Response {
  def apply(ahb: AhbLite3) = {
    val response = new AhbLite3Response(ahb.config)
    response.HRESP := ahb.HRESP
    response.HREADY := ahb.HREADY
    response.HRDATA := ahb.HRDATA
    response
  }

  def okayResponse(config: AhbLite3Config) = {
    val resp = new AhbLite3Response(config)
    resp.HRESP := False
    resp.HREADY := True
    resp.HRDATA.assignDontCare()
    resp
  }

  def stallResponse(config: AhbLite3Config) = {
    val resp = new AhbLite3Response(config)
    resp.HRESP := False
    resp.HREADY := False
    resp.HRDATA.assignDontCare()
    resp
  }
}

case class AhbLite3Response(config: AhbLite3Config) extends Bundle {
  val HREADY = Bool()
  val HRESP = Bool()
  val HRDATA = Bits(config.dataWidth bit)

  def drive(ahb: AhbLite3Master): Unit = {
    ahb.HREADY := HREADY
    ahb.HRESP := HRESP
    ahb.HRDATA := HRDATA
  }
}

case class AhbLite3Interconnect(
    masterPorts: Int = 2,
    slavePorts: Int = 1,
    decodings: Seq[SizeMapping]
) extends Component {
  val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 4)
  val io = new Bundle {
    val masters = Vec(slave(AhbLite3Master(ahbConfig)), masterPorts)
    val slaves = Vec(master(AhbLite3(ahbConfig)), slavePorts)
  }

  def priorityMux[T <: Data](selector: Bits, values: Seq[T], defaultValue: T = null) = {
    val selectors = values.zipWithIndex.map {
      case (v, idx) =>
        val literalString = ("0" * idx) + "1" + ("-" * (values.length - 1 - idx)) // TODO remove that ugly hack
        MaskedLiteral(literalString) -> v
    }

    if (defaultValue != null)
      selector.muxList(defaultValue, selectors)
    else
      selector.muxListDc(selectors)
  }

  val masterReq = IndexedSeq.fill(masterPorts) { Vec.fill(slavePorts) { Bool() } }
  val masterReqDel = IndexedSeq.tabulate(masterPorts) { i =>
    RegNext(masterReq(i), Vec.fill(slavePorts)(False))
  }
  val hold = Vec(Bool(), masterPorts)
  // who is currently in the dataphase?
  val activeArbitration = IndexedSeq.fill(slavePorts) { Vec.fill(masterPorts) { Bool() } }
  val slaveAdvancing = IndexedSeq.fill(slavePorts) { Bool() }
  // inverted / swivelled version of activeArbitration, for easier indexing on master side
  // inversion is done since spinal can't currently invert a whole Vec, remove when switching to Bits
  val masterInactiveArbitration = Seq.tabulate(masterPorts) { i =>
    Vec(activeArbitration.map(aa => !aa(i)))
  }

  // TODO add default slave
  // master side
  // we need to handle special cases:
  //   - no request is here and we need to provide the IDLE OKAY response (TODO: handle this via default slave?)
  //   - access has not yet been granted and we need to provide a response with HREADY = 0 -> hold=1
  //     detail: even if we have to hold because our new slave is not yet ready for us we have to
  //     provide the response from the previous slave, therefore we route HRESP if we are
  //     active somewhere, even while holding -> hold=0 even if otherwise 1
  val bufferedCtrl = Vec.tabulate(masterPorts) { i =>
    SkidBuffer(AhbLite3Control.apply(io.masters(i)), hold(i))
  }
  // TODO improve, need to reverse because of index handling difference between Vec & Bits
  val responseSel = Seq.tabulate(masterPorts) { i =>
    activeArbitration.map(_(i)).asBits().reversed
  }
  val muxedResponse = (0 until masterPorts).map { i =>
    priorityMux(
      responseSel(i),
      io.slaves.map(AhbLite3Response(_)).toSeq,
      defaultValue = AhbLite3Response.okayResponse(ahbConfig)
    )
  }
  val gatedResponse = (0 until masterPorts).map { i =>
    (hold(i) & masterInactiveArbitration(i).andR) ? AhbLite3Response.stallResponse(ahbConfig) | muxedResponse(
      i
    )
  }

  // gate the request: we dont want slave B getting a masters address phase signals while
  // it's still being stalled by slave A and couldn't continue
  val masterReqGated: IndexedSeq[Vec[Bool]] = (0 until masterPorts).map { i =>
    Vec(masterReq(i).map { _ & muxedResponse(i).HREADY })
  }

  (0 until masterPorts).foreach { i =>
    gatedResponse(i).drive(io.masters(i))
    masterReq(i) := Vec(decodings.map {
      _.hit(bufferedCtrl(i).HADDR) & (bufferedCtrl(i).HTRANS =/= 0)
    })
    hold(i) := (masterReqDel(i) & masterInactiveArbitration(i)).orR
  }

  // slave side
  // TODO evaluate strapping HSEL to high, always sending IDLE and therefore be able to rely on HREADY...
  val slaveActive = Vec(Reg(Bool()), slavePorts)
  val arbitratedReq = Vec(Vec(Bool(), masterPorts), slavePorts)
  val muxedControl = Vec(AhbLite3Control(ahbConfig), slavePorts)
  val muxedHWDATA = Vec(Bits(ahbConfig.dataWidth bit), slavePorts)
  for (i <- 0 until slavePorts) {
    // track if the slave is currently in a dataphase (might not be due to HSEL
    slaveActive(i).clearWhen(io.slaves(i).HREADYOUT).setWhen(io.slaves(i).HTRANS =/= 0).init(False)

    arbitratedReq(i) := OHMasking.first(masterReqGated.map { _(i) }).setName(f"arbitratedReq_$i")
    activeArbitration(i) := RegNextWhen(
      arbitratedReq(i),
      slaveAdvancing(i),
      Vec.fill(masterPorts)(False)
    )
    slaveAdvancing(i) := (io.slaves(i).HSEL.rise() && !slaveActive(i)) || (slaveActive(i) && io
      .slaves(i)
      .HREADYOUT)

    muxedControl(i) := MuxOH(arbitratedReq(i), bufferedCtrl.map(_.withoutOffset(decodings(i))))
    muxedHWDATA(i) := MuxOH(activeArbitration(i), io.masters.map { _.HWDATA })

    muxedControl(i).drive(io.slaves(i))
    io.slaves(i).HSEL := arbitratedReq(i).orR && (io.slaves(i).HTRANS =/= 0)
    io.slaves(i).HWDATA := muxedHWDATA(i)
    io.slaves(i).HREADY := io.slaves(i).HREADYOUT
  }
}
