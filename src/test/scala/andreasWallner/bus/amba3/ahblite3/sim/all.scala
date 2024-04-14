package andreasWallner.bus.amba3.ahblite3.sim

import andreasWallner.sim.{SimString, simLog, simLogId}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Master}

object HTRANS {
  val IDLE = 0
  val BUSY = 1
  val NONSEQ = 2
  val SEQ = 3
}

object HBURST {
  val SINGLE = 0
  val INCR = 1
  val WRAP4 = 2
  val INCR4 = 3
  val WRAP8 = 4
  val INCR8 = 5
  val WRAP16 = 6
  val INCR16 = 7
}

case class AhbLite3ControlSignals(
    address: BigInt,
    write: Boolean,
    size: Int,
    prot: Int,
    trans: Int,
    burst: Int,
    mastlock: Boolean,
) {
  override def toString = {
    val action = if (write) "W" else "R"
    val transStr = trans match {
      case HTRANS.IDLE   => "IDLE"
      case HTRANS.BUSY   => "BUSY"
      case HTRANS.NONSEQ => "NONSEQ"
      case HTRANS.SEQ    => "SEQ"
    }
    f"($transStr $address $action)"
  }
}

object AhbLite3ControlSignals {
  def fromBus(ahb: AhbLite3) = {
    AhbLite3ControlSignals(
      ahb.HADDR.toBigInt,
      ahb.HWRITE.toBoolean,
      ahb.HSIZE.toInt,
      ahb.HPROT.toInt,
      ahb.HTRANS.toInt,
      ahb.HBURST.toInt,
      ahb.HMASTLOCK.toBoolean
    )
  }

  def fromBus(ahb: AhbLite3Master) = {
    AhbLite3ControlSignals(
      ahb.HADDR.toBigInt,
      ahb.HWRITE.toBoolean,
      ahb.HSIZE.toInt,
      ahb.HPROT.toInt,
      ahb.HTRANS.toInt,
      ahb.HBURST.toInt,
      ahb.HMASTLOCK.toBoolean
    )
  }

  def fromBus(ahb: AhbWrapper) = {
    AhbLite3ControlSignals(
      ahb.HADDR.toBigInt,
      ahb.HWRITE.toBoolean,
      ahb.HSIZE.toInt,
      ahb.HPROT.toInt,
      ahb.HTRANS.toInt,
      ahb.HBURST.toInt,
      ahb.HMASTLOCK.toBoolean
    )
  }
}

abstract case class AhbLite3SlaveAgent(
    ahb: AhbLite3,
    cd: ClockDomain,
    ss: Option[SimString] = None,
    hreadyoutWhenIdle: Option[Boolean] = None
) {
  var delay = 0
  var addressPhase: Option[AhbLite3ControlSignals] = None
  def inDataPhase = addressPhase.isDefined

  // 7.1.2 "during reset all slaves must ensure that HREADYOUT is high"
  ahb.HREADYOUT #= true

  cd.onSamplings {
    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean) {
      assert(!inDataPhase, "HSEL & HREADY are high even though the slave is still stalling the bus")
      addressPhase = Some(AhbLite3ControlSignals.fromBus(ahb))
      delay = if (addressPhase.get.trans == 0) 0 else nextDelay()
    }

    ss.foreach { _ #= s"$delay $inDataPhase $addressPhase"}

    if (inDataPhase) {
      // HRESP might be left high from last random cycle before
      ahb.HRESP #= false

      // 3.2 "slave must provide zero wait OKAY response to IDLE transfer"
      // 3.2 "slave must provide zero wait OKAY response to BUSY transfer"
      simLog(s"AP = $addressPhase")
      if (Seq(HTRANS.IDLE, HTRANS.BUSY).contains(addressPhase.get.trans)) {
        ahb.HRESP #= false
        ahb.HREADYOUT #= true
        ahb.HRDATA.randomize()
        addressPhase = None
      } else if (Seq(HTRANS.SEQ, HTRANS.NONSEQ).contains(addressPhase.get.trans) && delay == 0) {
        // we enter this code at the start of the cycle that has HREADY=1
        val ap = addressPhase.get
        if (ap.write) {
          val resp = onWrite(ap.address, ahb.HWDATA.toBigInt)
          ahb.HRESP #= resp
        } else {
          val (value, resp) = onRead(ap.address)
          ahb.HRDATA #= value
          ahb.HRESP #= resp
        }

        ahb.HREADYOUT #= true
        addressPhase = None
      } else {
        // in a data-phase,
        ahb.HREADYOUT #= false
      }
    } else {
      ahb.HRDATA.randomize()
      ahb.HRESP.randomize()
      ahb.HREADYOUT #= hreadyoutWhenIdle.getOrElse(simRandom.nextBoolean())
    }
    // TODO verify that HWRITE stays constant during data phase
    // TODO can HREADYOUT be low before a transfer starts?? model does not do that currently
    delay = 0.max(delay - 1)
  }
  def nextDelay(): Int = simRandom.nextInt(3)
  def onRead(address: BigInt): (BigInt, Boolean)
  def onWrite(address: BigInt, value: BigInt): Boolean
}

// TODO handle early error response
// TODO add parameter to simulate interconnect (which may change control during "address phase")
abstract case class AhbLite3MasterAgent(ahb: AhbLite3Master, cd: ClockDomain, ss: Option[SimString]=None) {
  def setupNextTransfer(): (Option[BigInt], Boolean)
  def nextDelay(): Int = simRandom.nextInt(3)

  ahb.HTRANS #= HTRANS.IDLE
  ahb.HBURST #= 0

  var delay = nextDelay()
  var transferInfo: (Option[BigInt], Boolean) = (None, false)
  var inAddressPhase = false
  var inDataPhase = false
  var inBurst = false
  cd.onSamplings {
    if (inDataPhase && ahb.HREADY.toBoolean)
      inDataPhase = false

    if (inAddressPhase && !inDataPhase) {
      // watching HREADY for address phase is only a slave thing -> master must not look at it
      // - 3.1 states that address may be extended by previous bus transfer
      // - 3.6.1 when transfer is started HTRANS must stay constant
      // - 3.6.2 once HTRANS is nonseq, address must stay constant
      // - 4.1 slave samples address & control when HSEL && HREADY -> interconnect may change prio during wait
      // - 4.1.1 mandates a default slave if the memory space is not completely filled
      //         -> that must generate the mandated zero-delay ACK response
      //inBurst = (ahb.HTRANS.toInt == HTRANS.SEQ) || (ahb.HBURST.toInt != 0) // !! TODO this is late
      inBurst = transferInfo._2
      ahb.HTRANS #= (if (!inBurst) HTRANS.IDLE else HTRANS.BUSY)
      if (transferInfo._1.isDefined)
        ahb.HWDATA #= transferInfo._1.get
      //inBurst = transferInfo._2
      transferInfo = (None, false)

      inAddressPhase = false
      inDataPhase = true
    }

    ss.foreach { _ #= s"$delay $inAddressPhase $inDataPhase $inBurst" }

    if (delay == 0 && !inAddressPhase) {
      ahb.HTRANS #= HTRANS.NONSEQ
      ahb.HBURST #= 0
      ahb.HMASTLOCK #= false
      transferInfo = setupNextTransfer()
      if (transferInfo._2)
        inBurst = true
      delay = nextDelay()
      inAddressPhase = true
    } else if (!inAddressPhase) {
      // TODO Table 3-1 "When a master uses a BUSY transfer type the address and control signals must reflect the next transfer in the burst"
      // TODO 3.4 HSIZE must remain constant throughout a burst transfer
      // TODO Table 2-2 HWRITE must remain constant throughout a burst transfer
      // TODO 3.7 HPROT must remain constant throughout burst transfer

      // TODO while master is being stalled we first randomize for delay cycles
      // afterwards we stop randomizing - in which case HTRANS should also be randomized
      if (!inBurst) {
        ahb.HWRITE.randomize()
        ahb.HADDR.randomize()
        ahb.HSIZE.randomize()
        // ahb.HBURST.randomize() TODO reenable? we can't just randomize once we are in a burst
        // ahb.HMASTLOCK.randomize() TODO reenable? we can't just randomize once HMASTLOCK is enable?
        ahb.HPROT.randomize()
      }
      delay = 0.max(delay - 1)
    }

    if (!inDataPhase && !inBurst) { // TODO no need to keep stable through read (note in 3.1)
      ahb.HWRITE.randomize()
    }
  }
}

abstract case class AhbLite3SlaveMonitor(ahb: AhbLite3, cd: ClockDomain) {
  var addressPhase: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (addressPhase.isDefined && ahb.HREADY.toBoolean && addressPhase.get.trans != 0 && addressPhase.get.trans != 1) {
      val ap = addressPhase.get
      if (ap.write) {
        onWrite(ap.address, ahb.HWDATA.toBigInt)
      } else {
        onRead(ap.address, ahb.HRDATA.toBigInt)
      }
      addressPhase = None
    }

    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean) {
      addressPhase = Some(AhbLite3ControlSignals.fromBus(ahb))
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}

abstract case class AhbLite3MasterMonitor(ahb: AhbLite3Master, cd: ClockDomain, idx: Int) {
  var addressPhase: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (addressPhase.isDefined && Seq(HTRANS.IDLE, HTRANS.BUSY).contains(addressPhase.get.trans)) {
      addressPhase = None
    } else if (addressPhase.isDefined && ahb.HREADY.toBoolean) {
      val ap = addressPhase.get
      if (ap.write) {
        onWrite(ap.address, ahb.HWDATA.toBigInt)
      } else {
        onRead(ap.address, ahb.HRDATA.toBigInt)
      }
      addressPhase = None
    }

    if (addressPhase.isEmpty) {
      addressPhase = Some(AhbLite3ControlSignals.fromBus(ahb))
      //simLog(idx, addressPhase)
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}

abstract class AhbWrapper {
  def HTRANS: Bits
  def HADDR: UInt
  def HBURST: Bits
  def HMASTLOCK: Bool
  def HRDATA: Bits
  def HREADY: Bool
  def HRESP: Bool
  def HSIZE: Bits
  def HWDATA: Bits
  def HWRITE: Bool
  def HPROT: Bits
}

case class AhbLite3Wrapper(ahb: AhbLite3) extends AhbWrapper {
  def HTRANS = ahb.HTRANS
  def HADDR = ahb.HADDR
  def HBURST = ahb.HBURST
  def HMASTLOCK = ahb.HMASTLOCK
  def HRDATA = ahb.HRDATA
  def HREADY = ahb.HREADY
  def HRESP = ahb.HRESP
  def HSIZE = ahb.HSIZE
  def HWDATA = ahb.HWDATA
  def HWRITE = ahb.HWRITE
  def HPROT: Bits = ahb.HPROT
}

case class AhbLite3MasterWrapper(ahb: AhbLite3Master) extends AhbWrapper {
  def HTRANS = ahb.HTRANS
  def HADDR = ahb.HADDR
  def HBURST = ahb.HBURST
  def HMASTLOCK = ahb.HMASTLOCK
  def HRDATA = ahb.HRDATA
  def HREADY = ahb.HREADY
  def HRESP = ahb.HRESP
  def HSIZE = ahb.HSIZE
  def HWDATA = ahb.HWDATA
  def HWRITE = ahb.HWRITE
  def HPROT: Bits = ahb.HPROT
}

object AhbLite3ProtocolChecker {
  def apply(ahb: AhbLite3, cd: ClockDomain, id: String) =
    new AhbLite3ProtocolChecker(AhbLite3Wrapper(ahb), cd, id)
  def apply(ahb: AhbLite3Master, cd: ClockDomain, id: String) =
    new AhbLite3ProtocolChecker(AhbLite3MasterWrapper(ahb), cd, id)
}

class AhbLite3ProtocolChecker(ahb: AhbWrapper, cd: ClockDomain, id: String) {
  // check for immediate responses to IDLE/BUSY
  var transfer: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (transfer.isDefined && Seq(HTRANS.IDLE, HTRANS.BUSY).contains(transfer.get.trans)) {
      // 3.2 "Slave must always provide a zero wait state OKAY response to IDLE ...
      //     "Slave must always provide a zero wait state OKAY response to BUSY ...
      assert(
        ahb.HREADY.toBoolean,
        s"$id: IDLE transfer must get zero-wait state response, HREADY late"
      )
      assert(!ahb.HRESP.toBoolean, s"$id: IDLE transfer must get OKAY reponse, HRESP wrong")
      transfer = None
    } else if (transfer.isDefined && ahb.HREADY.toBoolean) {
      transfer = None
    }

    if (transfer.isEmpty) {
      transfer = Some(AhbLite3ControlSignals.fromBus(ahb))
    }
  }

  // TODO implement termination through slave error response
  // TODO implement multi-layer interconnect termination (3-11)
  var burst: Option[AhbLite3ControlSignals] = None
  var currentAddress: Option[AhbLite3ControlSignals] = None
  var remainingAddresses = 0
  var remainingData = 0
  cd.onSamplings {
    // undefined length burst is ended by a non-burst transfer...
    if (burst.isDefined && burst.get.burst == HBURST.INCR && Seq(HTRANS.IDLE, HTRANS.NONSEQ)
          .contains(ahb.HTRANS.toInt))
      burst = None

    if (burst.isDefined) {
      val current = AhbLite3ControlSignals.fromBus(ahb)
      // 3.5.1 after a burst has started, the master must not terminate the burst
      //       a master must not end a burst with a BUSY transfer for fixed length bursts
      val allowedStates = (burst.get.burst, remainingAddresses) match {
        case (HBURST.INCR, _) => Seq(HTRANS.SEQ, HTRANS.BUSY)
        case (_, 0) => Seq(HTRANS.IDLE, HTRANS.NONSEQ)
        case (_, _) => Seq(HTRANS.SEQ, HTRANS.BUSY)
      }
      simLogId(id)("checking for", allowedStates)
      assert(
        allowedStates.contains(ahb.HTRANS.toInt),
        s"""[${simTime()}] $id: invalid state during/after burst (${ahb.HTRANS.toInt} while ${allowedStates.mkString("/")} is allowed)"""
      )
      if(remainingAddresses != 0 || burst.get.burst == HBURST.INCR) {
        assert(current.burst == burst.get.burst, s"$id: HBURST changed during burst")
        assert(current.write == burst.get.write, s"$id: HWRITE changed during burst")
        assert(current.prot == burst.get.prot, s"$id: HPROT changed during burst")
        assert(current.size == burst.get.size, s"$id: HSIZE changed during burst")
        assert(current.mastlock == burst.get.mastlock, s"$id: HMASTLOCK changed during burst")
      }

      val unwrappedNextAddress = burst.get.address + (1 << burst.get.size)
      val nextAddress = if ((burst.get.burst & 1) == 1) {
        unwrappedNextAddress
      } else {
        val offsetBits = burst.get.size + (ahb.HBURST.toInt >> 1) + 1
        val boundary = 1 << offsetBits

        val static = (burst.get.address >> offsetBits) << offsetBits
        val dynamic = unwrappedNextAddress & (boundary - 1)
        static + dynamic
      }
      //TODO assert(current.address == nextAddress, f"$id: HADDR invalid during burst (${current.address}%x != $nextAddress%x)")

      if (currentAddress.isDefined && burst.get.burst != HBURST.INCR && ahb.HREADY.toBoolean) {
        remainingData = remainingData - 1
        currentAddress = None
        simLogId(id)(s"burst beat", remainingData)
      }
      if(id == "M0")
        simLogId(id)(currentAddress, ahb.HTRANS.toInt, remainingAddresses)
      if (currentAddress.isEmpty && ahb.HTRANS.toInt == HTRANS.SEQ) {
        currentAddress = Some(AhbLite3ControlSignals.fromBus(ahb))
        remainingAddresses = remainingAddresses - 1
        simLogId(id)("address phase", remainingAddresses)
      }
      if (remainingData == 0) {
        simLogId(id)(s"burst end")
        burst = None
      }
    }

    // in a separate if so that a burst after an undefined length burst is detected
    if (burst.isEmpty && ahb.HTRANS.toInt == HTRANS.NONSEQ && ahb.HBURST.toInt != HBURST.SINGLE && ahb.HREADY.toBoolean) {
      burst = Some(AhbLite3ControlSignals.fromBus(ahb))
      currentAddress = burst
      remainingData = 1 << ((ahb.HBURST.toInt >> 1) + 1)
      remainingAddresses = remainingData - 1
      simLogId(id)(s"start burst of $remainingData")
    }

    if (burst.isEmpty) {
      // TODO HTRANS checker
    }
  }
}
