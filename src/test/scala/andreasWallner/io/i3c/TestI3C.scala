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
    dut.io.data #= 0x55
    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.trigger #= false

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    dut.clockDomain.forkStimulus((100 MHz).toTime)

    val txScoreboard = ScoreboardInOrder[Event]()
    txScoreboard.pushRef(Byte(0x55))
    dut.clockDomain.waitSampling()
    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def event(e: Event) = {
        simLog(e)
        e match {
          case b: Byte => txScoreboard.pushDut(b)
          case _ =>
        }
      }
    }

    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSampling(10*10)
  }
}
