package andreasWallner.bus.amba3.ahblite3.sim

import andreasWallner.sim.SimString
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Master}

object HTRANS {
  val IDLE = 0
  val BUSY = 1
  val NONSEQ = 2
  val SEQ = 3
}

case class AhbLite3ControlSignals(
    address: BigInt,
    write: Boolean,
    size: Int,
    prot: Int,
    trans: Int
) {
  override def toString = {
    val action = if (write) "W" else "R"
    f"($address%04x $action)"
  }
}

abstract case class AhbLite3SlaveAgent(
    ahb: AhbLite3,
    cd: ClockDomain,
    ss: SimString,
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
      addressPhase = Some(
        AhbLite3ControlSignals(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
      delay = if (addressPhase.get.trans == 0) 0 else nextDelay()
    }
    if(ss != null)
      ss #= s"$delay $inDataPhase $addressPhase"
    if (inDataPhase) {
      // HRESP might be left high from last random cycle before
      ahb.HRESP #= false

      // 3.2 "slave must provide zero wait OKAY response to IDLE transfer"
      // 3.2 "slave must provide zero wait OKAY response to BUSY transfer"
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
abstract case class AhbLite3MasterAgent(ahb: AhbLite3Master, cd: ClockDomain, ss: SimString) {
  def setupNextTransfer(): Option[BigInt]
  def nextDelay(): Int = simRandom.nextInt(3)

  ahb.HTRANS #= HTRANS.IDLE
  ahb.HBURST #= 0

  var delay = nextDelay()
  var nextHWDATA: Option[BigInt] = None
  var inAddressPhase = false
  var inDataPhase = false
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
      ahb.HTRANS #= (if(ahb.HBURST.toInt == 0) HTRANS.IDLE else HTRANS.BUSY)
      if (nextHWDATA.isDefined)
        ahb.HWDATA #= nextHWDATA.get
      nextHWDATA = None

      inAddressPhase = false
      inDataPhase = true
    }

    if(ss != null)
      ss #= s"$delay $inAddressPhase $inDataPhase"
    if (delay == 0 && !inAddressPhase) {
      ahb.HTRANS #= HTRANS.NONSEQ
      ahb.HBURST #= 0
      ahb.HMASTLOCK #= false
      nextHWDATA = setupNextTransfer()
      delay = nextDelay()
      inAddressPhase = true
    } else if (!inAddressPhase) {
      // TODO while master is being stalled we first randomize for delay cycles
      // afterwards we stop randomizing - in which case HTRANS should also be randomized
      ahb.HWRITE.randomize()
      ahb.HADDR.randomize()
      ahb.HSIZE.randomize()
      // ahb.HBURST.randomize() TODO reenable? we can't just randomize once we are in a burst
      // ahb.HMASTLOCK.randomize() TODO reenable? we can't just randomize once HMASTLOCK is enable?
      ahb.HPROT.randomize()
      delay = 0.max(delay - 1)
    }

    if (!inDataPhase) { // TODO no need to keep stable through read (note in 3.1)
      ahb.HWRITE.randomize()
    }
  }
}

abstract case class AhbLite3SlaveMonitor(ahb: AhbLite3, cd: ClockDomain) {
  var addressPhase: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (addressPhase.isDefined && ahb.HREADY.toBoolean && addressPhase.get.trans != 0) {
      val ap = addressPhase.get
      if (ap.write) {
        onWrite(ap.address, ahb.HWDATA.toBigInt)
      } else {
        onRead(ap.address, ahb.HRDATA.toBigInt)
      }
      addressPhase = None
    }

    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean) {
      addressPhase = Some(
        AhbLite3ControlSignals(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}

abstract case class AhbLite3MasterMonitor(ahb: AhbLite3Master, cd: ClockDomain, idx: Int) {
  var addressPhase: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (addressPhase.isDefined && Seq(HTRANS.IDLE, HTRANS.BUSY).contains(addressPhase.get.trans)) {
      // 3.2 "Slave must always provide a zero wait state OKAY response to IDLE ...
      //     "Slave must always provide a zero wait state OKAY response to HTRANS ...
      assert(ahb.HREADY.toBoolean, "IDLE transfer must get zero-wait state response, HREADY late")
      assert(!ahb.HRESP.toBoolean, "IDLE transfer must get OKAY reponse, HRESP wrong")
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
      addressPhase = Some(
        AhbLite3ControlSignals(
          ahb.HADDR.toBigInt,
          ahb.HWRITE.toBoolean,
          ahb.HSIZE.toInt,
          ahb.HPROT.toInt,
          ahb.HTRANS.toInt
        )
      )
      //simLog(idx, addressPhase)
    }
  }

  def onRead(address: BigInt, value: BigInt): Unit
  def onWrite(address: BigInt, value: BigInt): Unit
}
