package andreasWallner.io.i3c

import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import andreasWallner.io.i3c.sim._
import andreasWallner.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}

import scala.language.postfixOps

class TestSdaIbi extends SpinalFunSuite {
  case class Fixture(dut: SdaTx, onAddress: (Int, Boolean, Boolean) => (Boolean, Seq[Int])) {
    val scoreboard = ScoreboardInOrder[Event]()

    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.stopCnt #= 10
    dut.io.trigger #= false
    dut.io.useRestart #= false
    dut.io.continueRx #= true

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    // fork and wait before setting up I3CSimTarget to avoid it seeing non-reset random values
    dut.clockDomain.forkStimulus((100 MHz).toTime)
    dut.clockDomain.waitSampling()

    FlowMonitor(dut.io.rxData, dut.clockDomain) { payload =>
      scoreboard.pushDut(Byte(payload.toInt))
    }

    val target = new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = onAddress(address, RnW, repeated)

      override def event(e: Event): Unit = {
        e match {
          case Bit(_) =>
          case _ => simLog("event", e)
        }
        e match {
          case Byte(_) | Start(_) | Stop() => scoreboard.pushDut(e)
          case Bit(_) =>
          case _ => fail("sim target saw unexpected event")
        }
      }
    }

    def finish(): Unit = {
      sleep(100)
      scoreboard.checkEmptyness()
    }
  }

  val dut = namedSimConfig.compile { SdaTx() }

  test(dut, "IBI w only mandatory byte") { dut =>
    SimTimeout((100 ns) * 8 * 15)
    val fixture = Fixture(dut, (_,_,_) => (false, Seq()))
    import fixture.{target, scoreboard}

    scoreboard.pushRef(Start(false))
    scoreboard.pushRef(Byte(0x85))
    scoreboard.pushRef(Byte(0x77))
    scoreboard.pushRef(Stop())

    sleep(timeToLong(100 ns))
    target.sendIBI(waitForStart = false, 0x42, Seq(0x77))

    FlowMonitor(dut.io.rxData, dut.clockDomain) { _ =>
      assert(dut.io.isIbi.toBoolean)
    }

    dut.clockDomain.waitSamplingWhere(!dut.io.idle.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }
}
