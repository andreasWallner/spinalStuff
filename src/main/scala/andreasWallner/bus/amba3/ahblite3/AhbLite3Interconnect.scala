package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._

import scala.language.postfixOps

object SkidBuffer {
  def apply[T <: Data](i: T, hold: Bool, init:T=null.asInstanceOf[T]) = {
    val buffer = new SkidBuffer(i)
    buffer.io.i := i
    buffer.io.hold := hold
    buffer.io.init := init
    buffer.io.o
  }
  def apply[T <: Data](payloadType: HardType[T], init:T) = {
    new SkidBuffer(payloadType)
  }
}

class SkidBuffer[T <: Data](payloadType: HardType[T])
    extends Component {
  val io = new Bundle {
    val i = in port payloadType()
    val o = out port payloadType()
    val hold = in port Bool()
    val init = in port payloadType()
  }

  val reg = RegNextWhen(io.i, !io.hold, init=io.init)
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

  def idleControl(config: AhbLite3Config) = {
    val ctrl = new AhbLite3Control(config)
    ctrl.HADDR := 0
    ctrl.HTRANS := 0
    ctrl.HPROT := 0
    ctrl.HBURST := 0
    ctrl.HMASTLOCK := False
    ctrl.HSIZE := 0
    ctrl.HWRITE := False
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
    resp.HRDATA := 0
    resp
  }

  def stallResponse(config: AhbLite3Config) = {
    val resp = new AhbLite3Response(config)
    resp.HRESP := False
    resp.HREADY := False
    resp.HRDATA := 0
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

object AhbLite3Interconnect {
  def apply(
    masterPorts: Int, 
    slavePorts: Int,
    decodings: Seq[SizeMapping],
    ahbConfig: AhbLite3Config = AhbLite3Config(addressWidth=16, dataWidth=4)) = 
      new AhbLite3Interconnect(decodings, Seq.fill(masterPorts)(Seq.tabulate(slavePorts){i => i}), ahbConfig)

  def calcSlaveConnectivity(masterConnectivity: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val slaveCount = masterConnectivity.flatMap(x=>x).max + 1
    Seq.tabulate(slaveCount) { slaveIdx =>
      masterConnectivity.zipWithIndex.collect {
        case (conn, masterIndex) if conn.contains(slaveIdx) => masterIndex
      }
    }
  }
}

class AhbLite3Interconnect(
    decodings: Seq[SizeMapping],
    masterConnectivity: Seq[Seq[Int]],
    ahbConfig: AhbLite3Config
) extends Component {
  val slavePorts = decodings.size
  val masterPorts = masterConnectivity.size
  val slaveConnectivity = AhbLite3Interconnect.calcSlaveConnectivity(masterConnectivity)
  val io = new Bundle {
    val masters = Vec(slave(AhbLite3Master(ahbConfig)), masterPorts)
    val slaves = Vec(master(AhbLite3(ahbConfig)), slavePorts)
  }

  // TODO investigate different options of implementing this
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

  val masterReq = Vec.tabulate(masterPorts){i => Bits(masterConnectivity(i).size bit)}
  val masterReqDel = masterReq.map(req => RegNext(req, init=B(0)))
  // who is currently in the dataphase?
  val activeArbitration = Vec.tabulate(slavePorts)(i => Bits(slaveConnectivity(i).size bit))
  val slaveAdvancing = Vec(Bool(), slavePorts)
  // transposed alias of activeArbitration, for easier indexing on master side
  val masterActiveArbitration = Vec.tabulate(masterPorts) { masterIndex =>
    Vec(masterConnectivity(masterIndex).map{ slaveIndex =>
      val arbitrationIndex = slaveConnectivity(slaveIndex).indexOf(masterIndex)
      activeArbitration(slaveIndex)(arbitrationIndex)
    }).asBits
  }

  // TODO add default slave
  // master side
  // we need to handle special cases:
  //   - no request is here and we need to provide the IDLE OKAY response
  //   - access has not yet been granted and we need to provide a response with HREADY = 0 -> hold=1
  //   - even if we have to hold because our new slave is not yet ready for us we have to
  //     provide the response from the previous slave, therefore we route HRESP if we are
  //     active somewhere, even while holding -> hold=0 even if otherwise 1
  val hold = Vec(Bool(), masterPorts)
  val bufferedCtrl = Vec.tabulate(masterPorts) { i =>
    SkidBuffer(AhbLite3Control.apply(io.masters(i)), hold(i), init=AhbLite3Control.idleControl(ahbConfig))
  }

  val responseSel = cloneOf(masterReq)
  val muxedResponse = Vec(AhbLite3Response(ahbConfig), masterPorts)
  // as long
  val gatedResponse = Vec(AhbLite3Response(ahbConfig), masterPorts)

  // gate the request: we dont want slave B getting a masters address phase signals while
  // it's still being stalled by slave A and couldn't continue
  val masterReqGated = cloneOf(masterReq)

  for (i <- 0 until masterPorts) {
    responseSel(i) := activeArbitration.map(_(i)).asBits()
    // reverse needed bec. auf Vec vs Bits ordering
    muxedResponse(i) := priorityMux(
      masterActiveArbitration(i),//responseSel(i),
      masterConnectivity(i).map(i => AhbLite3Response(io.slaves(i))).toSeq.reverse,
      defaultValue = AhbLite3Response.okayResponse(ahbConfig)
    )
    // TODO check if don't care here leads to useful hardware, otherwise connect HRDATA through
    // We need to block the response to the master until we have been arbitrated and the
    // slave has gotten the masters' address phase. Let that through though if we are
    // currently in a data phase with a previous slave
    gatedResponse(i) := (hold(i) & (~masterActiveArbitration(i)).andR) ?
      AhbLite3Response.stallResponse(ahbConfig) |
      muxedResponse(i)

    // don't request a slave until the last cycle of the data phase
    // with the previous slave - the second slave might be faster and
    // the master would not yet have data to provide
    masterReqGated(i) := masterReq(i) & B(masterConnectivity(i).size bits, default -> muxedResponse(i).HREADY)

    gatedResponse(i).drive(io.masters(i))
    masterReq(i) := Vec(masterConnectivity(i).map(slaveIdx => decodings(slaveIdx)).map {
      _.hit(bufferedCtrl(i).HADDR) & (bufferedCtrl(i).HTRANS =/= 0)
    }).asBits
    hold(i) := (masterReqDel(i) & ~masterActiveArbitration(i)).orR
  }

  // slave side
  // TODO evaluate strapping HSEL to high, always sending IDLE and therefore be able to rely on HREADY...
  // track if the slave is currently in a dataphase (might not be due to HSEL)
  val slaveActive = Vec(Reg(Bool()) init False, slavePorts)
  val arbitratedReq = cloneOf(activeArbitration)
  val muxedControl = Vec(AhbLite3Control(ahbConfig), slavePorts)
  val muxedHWDATA = Vec(Bits(ahbConfig.dataWidth bit), slavePorts)
  for (i <- 0 until slavePorts) {
    // track the state of the slave the same way the slave does it itself
    slaveActive(i).clearWhen(io.slaves(i).HREADYOUT).setWhen(io.slaves(i).HSEL && io.slaves(i).HREADY).init(False)

    arbitratedReq(i) := OHMasking.first(Vec(slaveConnectivity(i).map{masterIdx => 
      val reqIdx = masterConnectivity(masterIdx).indexOf(i)
      masterReqGated(masterIdx)(reqIdx)
    }).asBits)
    activeArbitration(i) := RegNextWhen(arbitratedReq(i), slaveAdvancing(i), B(0))
    // if the slave is inactive it's moving to a data phase if HSEL rises
    // if it's active then is moves to the next data phase when HREADYOUT is set
    slaveAdvancing(i) :=
      (!slaveActive(i) && io.slaves(i).HSEL.rise(initAt=False)) ||
      (slaveActive(i) && io.slaves(i).HREADYOUT)

    // TODO add option that sets defaults here for idle state -> less transitions
    muxedControl(i) := MuxOH(arbitratedReq(i), slaveConnectivity(i).map{masterIdx => bufferedCtrl(masterIdx).withoutOffset(decodings(i))})
    muxedHWDATA(i) := MuxOH(activeArbitration(i), slaveConnectivity(i).map{masterIdx => io.masters(masterIdx).HWDATA })

    muxedControl(i).drive(io.slaves(i))
    io.slaves(i).HSEL := arbitratedReq(i).orR && (io.slaves(i).HTRANS =/= 0)
    io.slaves(i).HWDATA := muxedHWDATA(i)
    // Provide HREADY if the slave is not active, the specifiction does not require
    // specific behaviour in this case. If we don't do this a slave that generates a
    // low HREADYOUT would deadlock itself.
    io.slaves(i).HREADY := io.slaves(i).HREADYOUT || !slaveActive(i)
  }
}
