package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._

import scala.language.postfixOps

object SkidBuffer {
  def apply[T <: Data](payloadType: HardType[T]) = {
    new SkidBuffer(payloadType, null.asInstanceOf[T])
  }
  def apply[T <: Data](payloadType: HardType[T], init: => T) = {
    new SkidBuffer(payloadType, init)
  }
  def apply[T <: Data](i: T, hold: Bool, init: => T = null.asInstanceOf[T]) = {
    val buffer = new SkidBuffer(i, init)
    buffer.io.i := i
    buffer.io.hold := hold
    buffer.io.o
  }
}

/** A buffer that is either transparent (comb. pass-through) or holds the previous input value
 *
 * If io.hold === False, io.o tracks io.i w/o delay
 * If io.hold === True, io.o will stay at the last value io.i has while io.hold === False
 *
 * Or: a register & a mux
 * */
class SkidBuffer[T <: Data](payloadType: HardType[T], init: => T) extends Component {
  val io = new Bundle {
    val i = in port payloadType()
    val o = out port payloadType()
    val hold = in port Bool()
  }

  val reg = RegNextWhen(io.i, !io.hold, init = init)
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
    ctrl.HTRANS := 0
    ctrl.HSIZE := 0
    ctrl.HADDR := 0
    ctrl.HWRITE := False
    ctrl.HBURST := 0
    ctrl.HPROT := 0
    ctrl.HMASTLOCK := False
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
      decodings: Seq[SizeMapping],
      ahbConfig: AhbLite3Config
  ) =
    new AhbLite3Interconnect(
      decodings.map(d => (d, ahbConfig)),
      Seq.fill(masterPorts)(decodings.indices),
      ahbConfig
    )

  def apply(
      decodings: Seq[SizeMapping],
      masterConnectivity: Seq[Seq[Int]],
      ahbConfig: AhbLite3Config
  ) =
    new AhbLite3Interconnect(decodings.map(d => (d, ahbConfig)), masterConnectivity, ahbConfig)

  def calcSlaveConnectivity(masterConnectivity: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val slaveCount = masterConnectivity.flatten.max + 1
    Seq.tabulate(slaveCount) { slaveIdx =>
      masterConnectivity.zipWithIndex.collect {
        case (conn, masterIndex) if conn.contains(slaveIdx) => masterIndex
      }
    }
  }
}

class AhbLite3Interconnect(
    decodings: Seq[(SizeMapping, AhbLite3Config)],
    masterConnectivity: Seq[Seq[Int]],
    ahbConfig: AhbLite3Config
) extends Component {
  val slavePorts = decodings.size
  val masterPorts = masterConnectivity.size
  val slaveConnectivity = AhbLite3Interconnect.calcSlaveConnectivity(masterConnectivity)
  val io = new Bundle {
    val masters = Vec(slave(AhbLite3Master(ahbConfig)), masterPorts)
    val slaves = Vec(decodings.map{ case (_, config) => master(AhbLite3(config))})
  }

  // TODO investigate different options of implementing this
  def priorityMux[T <: Data](selector: Bits, values: Seq[T], defaultValue: T = null) = {
    val selectors = values.zipWithIndex.map {
      case (v, idx) =>
        val bit = selector.getWidth - 1 - idx
        val ones = (BigInt(1) << selector.getWidth) - 1
        new MaskedLiteral(BigInt(1) << bit, (ones << bit) & ones, selector.getWidth) -> v
    }

    if (defaultValue != null)
      selector.muxList(defaultValue, selectors)
    else
      selector.muxListDc(selectors)
  }

  val masterReq = Vec.tabulate(masterPorts) { i =>
    Bits(masterConnectivity(i).size bit)
  }
  val masterReqDel = masterReq.map(req => RegNext(req, init = B(0)))
  // who is currently in the dataphase?
  val activeArbitration = Vec.tabulate(slavePorts)(i => Bits(slaveConnectivity(i).size bit))
  val slaveAdvancing = Vec(Bool(), slavePorts)
  // transposed alias of activeArbitration, for easier indexing on master side
  val masterActiveArbitration = Vec.tabulate(masterPorts) { masterIndex =>
    Vec(masterConnectivity(masterIndex).map { slaveIndex =>
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
    val sb = SkidBuffer(AhbLite3Control(io.masters(i)), AhbLite3Control.idleControl(ahbConfig))
    sb.io.i := AhbLite3Control.apply(io.masters(i))
    sb.io.hold := hold(i)
    sb.io.o
  }

  val responseSel = cloneOf(masterReq)
  val muxedResponse = Vec(AhbLite3Response(ahbConfig), masterPorts)
  // as long
  val gatedResponse = Vec(AhbLite3Response(ahbConfig), masterPorts)

  // gate the request: we don't want slave B getting a masters address phase signals while
  // it's still being stalled by slave A and is maybe not able continue with B
  val masterReqGated = cloneOf(masterReq)

  for (i <- 0 until masterPorts) {
    responseSel(i) := masterConnectivity(i)
      .map { slaveIdx =>
        val arbIdx = slaveConnectivity(slaveIdx).indexOf(i)
        activeArbitration(slaveIdx)(arbIdx)
      }
      .asBits()
    // reverse needed bec. auf Vec vs Bits ordering
    muxedResponse(i) := priorityMux(
      masterActiveArbitration(i), //responseSel(i),
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
    masterReqGated(i) := masterReq(i) & B(
      masterConnectivity(i).size bits,
      default -> muxedResponse(i).HREADY
    )

    gatedResponse(i).drive(io.masters(i))
    masterReq(i) := Vec(masterConnectivity(i).map(slaveIdx => decodings(slaveIdx)).map {
      _._1.hit(bufferedCtrl(i).HADDR) & (bufferedCtrl(i).HTRANS =/= 0)
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
    slaveActive(i)
      .clearWhen(io.slaves(i).HREADYOUT)
      .setWhen(io.slaves(i).HSEL && io.slaves(i).HREADY)
      .init(False)

    arbitratedReq(i) := OHMasking.first(Vec(slaveConnectivity(i).map { masterIdx =>
      val reqIdx = masterConnectivity(masterIdx).indexOf(i)
      masterReqGated(masterIdx)(reqIdx)
    }).asBits)
    activeArbitration(i) := RegNextWhen(arbitratedReq(i), slaveAdvancing(i), B(0))
    // if the slave is inactive it's moving to a data phase if HSEL rises
    // if it's active then is moves to the next data phase when HREADYOUT is set
    slaveAdvancing(i) :=
      (!slaveActive(i) && io.slaves(i).HSEL.rise(initAt = False)) ||
      (slaveActive(i) && io.slaves(i).HREADYOUT)

    // TODO add option that sets defaults here for idle state -> less transitions
    muxedControl(i) := MuxOH(arbitratedReq(i), slaveConnectivity(i).map { masterIdx =>
      bufferedCtrl(masterIdx).withoutOffset(decodings(i)._1, decodings(i)._2)
    })
    muxedHWDATA(i) := MuxOH(activeArbitration(i), slaveConnectivity(i).map { masterIdx =>
      io.masters(masterIdx).HWDATA
    })

    muxedControl(i).drive(io.slaves(i))
    io.slaves(i).HSEL := arbitratedReq(i).orR && (io.slaves(i).HTRANS =/= 0)
    io.slaves(i).HWDATA := muxedHWDATA(i)
    // Provide HREADY if the slave is not active, the specifiction does not require
    // specific behaviour in this case. If we don't do this a slave that generates a
    // low HREADYOUT would deadlock itself.
    io.slaves(i).HREADY := io.slaves(i).HREADYOUT || !slaveActive(i)
  }
}
