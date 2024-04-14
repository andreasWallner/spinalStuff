package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config}

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
  * - locked transfer locks for one cycle too long, to reuse hardware present from burst
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
      val locked = RegInit(False)
      val slaveAdvancing = Bool()

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
      val realDataPhaseActive = Reg(Bool())
        .clearWhen(io.output.HREADYOUT)
        .setWhen(io.output.HSEL && io.output.HREADY && io.output.HTRANS(1))
        .init(False)
      slaveAdvancing :=
        ((!dataPhaseActive && io.output.HSEL.rise(initAt = False)) ||
          (dataPhaseActive && io.output.HREADYOUT)) // && !locked

      val lastArea = new Area {
        val inUndefinedBurst = Reg(Bool()) init False
        inUndefinedBurst
          .clearWhen(slaveAdvancing && (io.output.HTRANS === 0 || io.output.HTRANS === 2))
          .setWhen(slaveAdvancing && io.output.HTRANS === 2 && io.output.HBURST === 1)
        val endOfUndefinedBurst = inUndefinedBurst && (io.output.HTRANS === 0 || io.output.HTRANS === 2)

        val beatCounter = Reg(UInt(4 bits)) init 0
        val lastBeat = Vec(U(0), U(3), U(7), U(15))(U(io.output.HBURST >> 1))
        val isLast = (lastBeat === beatCounter && io.output.HBURST =/= 1) || io.output.ERROR

        when(slaveAdvancing && io.output.HBURST =/= 1 && io.output.HTRANS(1)) {
          beatCounter := beatCounter + 1
          when(io.output.HTRANS === 2) {
            beatCounter := 1
          }
          when(isLast) {
            beatCounter := 0
          }
        }
      }

      val maskProposal = Bits(inputsCount bits) // which master will be next
      val maskArbitrated =
        RegNextWhen(maskProposal, slaveAdvancing && (!locked | lastArea.isLast), init = B(0)) // which master is driving the dataphase

      locked
        .clearWhen(lastArea.endOfUndefinedBurst)
        .setWhen(
          slaveAdvancing && io.output.HTRANS =/= 0 && (io.output.HBURST =/= 0 || io.output.HMASTLOCK)
        )
        .clearWhen(
          (io.output.HBURST === 0 && !io.output.HMASTLOCK) || (lastArea.isLast && io.output
            .HTRANS(1) && io.output.HREADYOUT && !io.output.HMASTLOCK)
        )

      val requests = bufferedCtrl.map(i => i.HSEL && i.HTRANS(1)).asBits // which master requests, masterReq
      // next master being selected

      def roundRobin[T <: Data](requests: T, ohPriority: T): T =
        new Composite(requests, "roundRobin") {
          val width = requests.getBitsWidth
          val uRequests = requests.asBits.asUInt
          val uGranted = ohPriority.asBits.asUInt

          val doubleRequests = uRequests @@ uRequests
          val sub = ~(doubleRequests - uGranted)
          val doubleGrant = doubleRequests & sub
          val masked = doubleGrant(width, width bits) | doubleGrant(0, width bits)

          val ret = cloneOf(requests)
          ret.assignFromBits(masked.asBits)
        }.ret

      val nextProposal = if (roundRobinArbiter) {
        val nextPrio = maskArbitrated(maskArbitrated.high - 1 downto 0) ## maskArbitrated.msb
        val arbiterPrio = RegNextWhen(nextPrio, nextPrio.orR && locked) init B(
          maskArbitrated.getWidth bit,
          maskArbitrated.high -> True,
          default -> False
        )
        roundRobin(requests, arbiterPrio)
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
      io.output.HSEL := bufferedCtrl(requestIndex).HSEL // TODO don't select if there is no transfer and no lock!
      io.output.HADDR := bufferedCtrl(requestIndex).HADDR
      io.output.HWRITE := bufferedCtrl(requestIndex).HWRITE
      io.output.HSIZE := bufferedCtrl(requestIndex).HSIZE
      io.output.HBURST := bufferedCtrl(requestIndex).HBURST
      io.output.HPROT := bufferedCtrl(requestIndex).HPROT
      io.output.HTRANS := io.output.HSEL ? bufferedCtrl(requestIndex).HTRANS | B"00"
      io.output.HMASTLOCK := bufferedCtrl(requestIndex).HMASTLOCK

      // Provide HREADY if the slave is not active, the specifiction does not require
      // specific behaviour in this case. If we don't do this a slave that generates a
      // low HREADYOUT would deadlock itself.
      io.output.HREADY := io.output.HREADYOUT || !dataPhaseActive

      val dataIndex = RegNextWhen(requestIndex, slaveAdvancing)
      io.output.HWDATA := io.inputs(dataIndex).HWDATA

      // generate IDLE responses and HREADYOUT while input does not drive
      // a transaction
      bufferedCtrl.lazyZip(io.inputs).lazyZip(maskArbitrated.asBools).zipWithIndex.foreach {
        case ((buffered, input, requestRouted), idx) =>
          val was_idle = RegNextWhen(buffered.HTRANS === 0, slaveAdvancing) init False
          val last_hsel = RegNext(buffered.HSEL, slaveAdvancing) init True
          val stall = hold(idx) & !maskArbitrated(idx)
          val idle_resp = RegNext(buffered.HTRANS === 0 && buffered.HSEL && !hold(idx)) init True
          idle_resp.setName("idle_resp" + input.getName())
          input.HRDATA := io.output.HRDATA
          input.HRESP := idle_resp ? False | io.output.HRESP
          input.HREADYOUT := !stall && ((!requestRouted && !buffered.HSEL) || (requestRouted && io.output.HREADYOUT) || (was_idle && last_hsel))
      }

    }
}
