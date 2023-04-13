package andreasWallner.io.i3c

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import andreasWallner.io.i3c.sim._
import andreasWallner.SpinalFunSuite
import andreasWallner.sim._

import scala.language.postfixOps

class TestSdaTx extends SpinalFunSuite {
  var debugString: Bits = null
  def assignString(s: String): Unit = {
    simLog(debugString)
    var toAssignBI = BigInt(0)
    s.take(20).padTo(20, ' ').foreach(c => toAssignBI = (toAssignBI << 8) + c)
    debugString #= toAssignBI
  }
  val dut = SimConfig.withFstWave.compile {
    val comp = SdaTx()
    comp.rework {
      val debug = Reg(Bits(20 bit))
      debug.simPublic()
      debugString = debug
    }
    comp
  }

  test(dut, "SDA TX nack -> S") { dut =>
    dut.io.data #= 0x54
    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.stopCnt #= 10
    dut.io.trigger #= false
    dut.io.useRestart #= false

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    dut.clockDomain.forkStimulus((100 MHz).toTime)

    val txScoreboard = ScoreboardInOrder[Event]()
    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Stop())
    dut.clockDomain.waitSampling()
    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = (false, Seq())
      override def event(e: Event): Unit = {
        simLog(e)
        e match {
          case Byte(_) | Start(_) | Stop() => txScoreboard.pushDut(e)
          case Bit(_)                      =>
          case _                           => fail("sim target saw unexpected event")
        }
      }
    }

    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(!dut.io.idle.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    sleep(100)

    txScoreboard.checkEmptyness()
  }

  test(dut, "SDA TX nack -> R") { dut =>
    dut.io.data #= 0x54
    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.stopCnt #= 10
    dut.io.trigger #= false
    dut.io.useRestart #= true

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    dut.clockDomain.forkStimulus((100 MHz).toTime)

    val txScoreboard = ScoreboardInOrder[Event]()
    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Start(repeated = true))
    txScoreboard.pushRef(Byte(0x43))
    txScoreboard.pushRef(Stop())
    dut.clockDomain.waitSampling()
    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = (false, Seq())

      override def event(e: Event): Unit = {
        simLog(e)
        e match {
          case Byte(_) | Start(_) | Stop() => txScoreboard.pushDut(e)
          case Bit(_) =>
          case _ => fail("sim target saw unexpected event")
        }
      }
    }

    dut.io.trigger.strobe(dut.clockDomain)
    dut.clockDomain.waitSamplingWhere(!dut.io.idle.toBoolean)
    // assign data to be sent as control SM would
    dut.io.data #= 0x77
    dut.clockDomain.waitSamplingWhere(dut.io.ackStrb.toBoolean)
    assert(!dut.io.ack.toBoolean)
    // assign new address
    dut.io.data #= 0x43
    dut.io.useRestart #= false
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    sleep(100)

    txScoreboard.checkEmptyness()
  }

  test(dut, "SDA TX ack -> 1 byte -> S") { dut =>
    dut.io.data #= 0x54
    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.stopCnt #= 10
    dut.io.trigger #= false
    dut.io.useRestart #= false

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    dut.clockDomain.forkStimulus((100 MHz).toTime)

    val txScoreboard = ScoreboardInOrder[Event]()
    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Byte(0x77))
    txScoreboard.pushRef(Stop())
    dut.clockDomain.waitSampling()
    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = (true, Seq())

      override def event(e: Event): Unit = {
        simLog(e)
        e match {
          case Byte(_) | Start(_) | Stop() => txScoreboard.pushDut(e)
          case Bit(_) =>
          case _ => fail("sim target saw unexpected event")
        }
      }
    }
    // TODO next step: implement ACK from target
    dut.io.trigger.strobe(dut.clockDomain)
    dut.clockDomain.waitSamplingWhere(!dut.io.idle.toBoolean)
    dut.io.data #= 0x77
    dut.io.trigger #= true
    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.trigger #= false
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    sleep(100)

    txScoreboard.checkEmptyness()
  }
}
