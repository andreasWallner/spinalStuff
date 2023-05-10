package andreasWallner.bus.amba3.ahblite3.sim

import andreasWallner.sim.SimString
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Master}

import scala.util.Random

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

abstract case class AhbLite3SlaveAgent(ahb: AhbLite3, cd: ClockDomain, ss: SimString) {
  var delay = 0
  var addressPhase: Option[AhbLite3ControlSignals] = None
  def inDataPhase = addressPhase.isDefined

  // 7.1.2 "during reset all slaves must ensure that HREADYOUT is high"
  ahb.HREADYOUT #= true

  cd.onSamplings {
    if (ahb.HSEL.toBoolean && ahb.HREADY.toBoolean) {
      assert(!inDataPhase, "HSEL & HREADY are high even though the slave is still stalling the bus")
      assert(Seq(0, 2).contains(ahb.HTRANS.toInt), "agent only supports IDLE/NONSEQ transfers ATM")
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
    ss #= s"$delay $inDataPhase $addressPhase"
    if (inDataPhase) {
      // HRESP might be left high from last random cycle before
      ahb.HRESP #= false

      // 3.2 "slave must provide zero wait OKAY response to IDLE transfer"
      if (addressPhase.get.trans == 0) {
        ahb.HRESP #= false
        ahb.HREADYOUT #= true
        ahb.HRDATA.randomize()
        addressPhase = None
      } else if (addressPhase.get.trans == 2 && delay == 0) {
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
      ahb.HREADYOUT #= false
    }
    // TODO verify that HWRITE stays constant during data phase
    // TODO can HREADYOUT be low before a transfer starts?? model does not do that currently
    delay = 0.max(delay - 1)
  }
  def nextDelay(): Int = Random.nextInt(3)
  def onRead(address: BigInt): (BigInt, Boolean)
  def onWrite(address: BigInt, value: BigInt): Boolean
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

// TODO handle early error response
// TODO add parameter to simulate interconnect (which may change control during "address phase"
abstract case class AhbLite3MasterAgent(ahb: AhbLite3Master, cd: ClockDomain, ss: SimString) {
  def setupNextTransfer(): Option[BigInt]
  def nextDelay(): Int = Random.nextInt(3)

  ahb.HTRANS #= 0 // IDLE
  ahb.HBURST #= 0
  var delay = nextDelay()
  var nextHWDATA: Option[BigInt] = None
  var inAddressPhase = false
  var inDataPhase = false
  cd.onSamplings {
    //delay = 0.max(delay - 1)

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
      ahb.HTRANS #= 0 // IDLE
      if (nextHWDATA.isDefined)
        ahb.HWDATA #= nextHWDATA.get
      nextHWDATA = None

      inAddressPhase = false
      inDataPhase = true
    }

    if (delay == 0 && !inAddressPhase) {
      ahb.HTRANS #= 2 // NONSEQ
      ahb.HBURST #= 0
      ahb.HMASTLOCK #= false
      //ahb.HSIZE #= 2
      nextHWDATA = setupNextTransfer()
      //ahb.HWRITE #= nextHWRITE.isDefined
      delay = nextDelay()
      inAddressPhase = true
    } else if (!inAddressPhase) {
      ahb.HWRITE.randomize()
      ahb.HADDR.randomize()
      ahb.HSIZE.randomize()
      ahb.HBURST.randomize()
      ahb.HMASTLOCK.randomize()
      ahb.HPROT.randomize()
      delay = 0.max(delay - 1)
    }

    if (!inDataPhase) { // TODO no need to keep stable through read (note in 3.1)
      ahb.HWRITE.randomize()
    }
    ss #= s"$delay $inAddressPhase $inDataPhase"
  }
}

abstract case class AhbLite3MasterMonitor(ahb: AhbLite3Master, cd: ClockDomain, idx: Int) {
  var addressPhase: Option[AhbLite3ControlSignals] = None
  cd.onSamplings {
    if (addressPhase.isDefined && addressPhase.get.trans == 0) {
      // 3.2 "Slave must always provide a zero wait state OKAY response to IDLE ...
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
      assert(Seq(0,2).contains(ahb.HTRANS.toInt), "only NONSEQ is currently supported")
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

