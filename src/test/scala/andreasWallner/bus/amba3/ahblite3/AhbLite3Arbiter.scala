package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config}
import spinal.lib.bus.bram.BRAM

import scala.language.postfixOps

case class AhbLite3Control(config: AhbLite3Config) extends Bundle with IMasterSlave {
  val HADDR = UInt(config.addressWidth bits)
  val HSEL = Bool()
  val HREADY = Bool()
  val HWRITE = Bool()
  val HSIZE = Bits(3 bit)
  val HBURST = Bits(3 bit)
  val HPROT = Bits(4 bit)
  val HTRANS = Bits(2 bit)
  val HMASTLOCK = Bool()

  override def asMaster(): Unit = {
    out(HADDR, HSEL, HWRITE, HSIZE, HBURST, HPROT, HTRANS, HMASTLOCK)
  }

  def >>(ahb: AhbLite3): Unit = {
    ahb.HADDR := HADDR
    ahb.HSEL := HSEL
    ahb.HWRITE := HWRITE
    ahb.HSIZE := HSIZE
    ahb.HBURST := HBURST
    ahb.HPROT := HPROT
    ahb.HTRANS := HTRANS
    ahb.HMASTLOCK := HMASTLOCK
  }
}
object AhbLite3Control {
  def apply(ahb: AhbLite3): AhbLite3Control = {
    val control = AhbLite3Control(ahb.config)
    control.HADDR := ahb.HADDR
    control.HSEL := ahb.HSEL
    control.HWRITE := ahb.HWRITE
    control.HSIZE := ahb.HSIZE
    control.HBURST := ahb.HBURST
    control.HPROT := ahb.HPROT
    control.HTRANS := ahb.HTRANS
    control.HMASTLOCK := ahb.HMASTLOCK
    control
  }

  def idleControl(config: AhbLite3Config) = {
    val control = AhbLite3Control(config)
    control.HADDR.assignDontCare()
    control.HSEL := False
    control.HWRITE.assignDontCare()
    control.HSIZE.assignDontCare()
    control.HBURST := B"000"
    control.HPROT := B"0000"
    control.HTRANS := B"00"
    control.HMASTLOCK := False
    control
  }
}

/**
  * AHB Lite arbiter
  *
  * Design details:
  * - arbiter doesn't not modify any transfers, burst locks slave to master
  *
  * @param ahbLite3Config : Ahb bus configuration
  * @param inputsCount    : Number of inputs for the arbiter
  */
case class AhbLite3Arbiter(
    ahbLite3Config: AhbLite3Config,
    inputsCount: Int,
    roundRobinArbiter: Boolean = true
) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(AhbLite3(ahbLite3Config)), inputsCount)
    val output = master(AhbLite3(ahbLite3Config))
  }

  val logic = if (inputsCount == 1) new Area {
    io.output << io.inputs.head
  }
  else
    new Area {
      val hold = Vec(Bool(), inputsCount)

      // skidbuffers (comb connection or last buffered value) for all requests
      val bufferedCtrl = Vec.tabulate(inputsCount) { i =>
        SkidBuffer(
          AhbLite3Control(io.inputs(i)),
          hold(i),
          AhbLite3Control.idleControl(ahbLite3Config)
        )
      }

      val dataPhaseActive = Reg(Bool())
        .clearWhen(io.output.HREADYOUT)
        .setWhen(io.output.HSEL && io.output.HREADY)
        .init(False)
      // do not look at io.output.HREADY as slave does not need to drive it high
      // if not selected
      val slaveAdvancing =
        (!dataPhaseActive && io.output.HSEL.rise(initAt = False)) ||
          (dataPhaseActive && io.output.HREADYOUT)

      // if we keep track of a burst by looking at HTRANS & NONSEQ/SEQ then
      // TODO option get rid of this, use HTRANS NONSEQ/SEQ difference to keep track of burst
      val burst = new Area {
        val inUndefinedBurst = Reg(Bool()) init False
        inUndefinedBurst
          .clearWhen(slaveAdvancing && (io.output.HTRANS === 0 || io.output.HTRANS === 2))
          .setWhen(slaveAdvancing && io.output.HTRANS === 2 && io.output.HBURST === 1)
        val endOfUndefinedBurst = inUndefinedBurst && (io.output.HTRANS === 0 || io.output.HTRANS === 2)

        val beatCounter = Reg(UInt(4 bits)) init 0
        val lastBeat = Vec(U(0), U(3), U(7), U(15))(U(io.output.HBURST >> 1))
        // TODO why can't we use inUndefinedBurst here?
        val isLast = (lastBeat === beatCounter && io.output.HBURST =/= 1) || io.output.ERROR

        when(slaveAdvancing && io.output.HBURST =/= 1 && io.output.HTRANS(1)) {
          beatCounter := beatCounter + 1
          // TODO get rid of this check
          when(io.output.HTRANS === 2) {
            beatCounter := 1
          }
          when(isLast) {
            beatCounter := 0
          }
        }
      }

      // lock to stay with one master
      val locked = RegInit(False)
      // proposal for the next master, and the master that is driving the current address phase
      val maskProposal = Bits(inputsCount bits)
      val maskArbitrated =
        RegNextWhen(maskProposal, slaveAdvancing && (!locked | burst.isLast), init = B(0))

      locked
        .clearWhen(burst.endOfUndefinedBurst)
        .setWhen(
          slaveAdvancing && io.output.HTRANS(1) && (io.output.HBURST =/= 0 || io.output.HMASTLOCK)
        )
        .clearWhen(io.output.HREADYOUT && !io.output.HTRANS(0) && !io.output.HMASTLOCK)

      // OH vector of which masters are requesting a transaction
      // we don't need this to be 1 during a burst (because of lock), so only look at bit 1 to save on resources
      val requests = bufferedCtrl.map(i => i.HSEL && i.HTRANS(1)).asBits

      val nextProposal = if (roundRobinArbiter) {
        val nextPrio = maskArbitrated.rotateLeft(1)
        val arbiterPrio = RegNextWhen(nextPrio, nextPrio.orR && !locked) init B(
          maskArbitrated.getWidth bit,
          maskArbitrated.high -> True,
          default -> False
        )
        OHMasking.roundRobin(requests, arbiterPrio)
      } else {
        OHMasking.first(requests)
      }

      when(locked) {
        maskProposal := maskArbitrated
      } otherwise {
        maskProposal := nextProposal
      }
      val requestsDel = RegNext(requests) init 0

      // hold if request will not be next
      hold.zipWithIndex.foreach { case (b, idx) => b := requestsDel(idx) && ~maskArbitrated(idx) }

      val requestIndex = OHToUInt(maskProposal)
      bufferedCtrl(requestIndex) >> io.output
      io.output.HSEL.allowOverride() := True

      // Provide HREADY if the slave is not active, the specifiction does not require
      // specific behaviour in this case. If we don't do this a slave that generates a
      // low HREADYOUT would deadlock itself.
      io.output.HREADY := io.output.HREADYOUT || !dataPhaseActive

      val dataIndex = RegNextWhen(requestIndex, slaveAdvancing)
      io.output.HWDATA := io.inputs(dataIndex).HWDATA

      // generate IDLE responses and HREADYOUT while input does not drive a transaction
      bufferedCtrl.lazyZip(io.inputs).lazyZip(maskArbitrated.asBools).zipWithIndex.foreach {
        case ((buffered, input, requestRouted), idx) =>
          val was_idle = RegNextWhen(buffered.HTRANS === 0, slaveAdvancing) init False
          val last_hsel = RegNext(buffered.HSEL, slaveAdvancing) init True
          val stall = hold(idx) & !maskArbitrated(idx)
          // do not do the idle response, if the previous transfer is stuck in the data phase (hold)
          val idle_resp = RegNext(buffered.HTRANS === 0 && buffered.HSEL && !hold(idx)) init True
          idle_resp.setName("idle_resp" + input.getName())
          input.HRDATA := io.output.HRDATA
          input.HRESP := idle_resp ? False | io.output.HRESP
          // TODO optimize this
          input.HREADYOUT := !stall && ((!requestRouted && !buffered.HSEL) || (requestRouted && io.output.HREADYOUT) || (was_idle && last_hsel))
      }

    }
}
