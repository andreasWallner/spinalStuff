package andreasWallner.io.i3c

import andreasWallner.SpinalFunSuite
import andreasWallner.io.i3c.sim._
import andreasWallner.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._

import scala.language.postfixOps

class TestSdaRx extends SpinalFunSuite {
  case class Fixture(dut: SdaTx, onAddress: (Int, Boolean, Boolean) => (Boolean, Seq[Int]) ) {
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

    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = onAddress(address, RnW, repeated)

      override def event(e: Event): Unit = {
        e match {
          case Bit(_) =>
          case _ => //simLog(e)
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

  test(dut, "SDA RX ack -> 1 byte -> P") { dut =>
    SimTimeout((100 ns) * 8 * 5)
    val fixture = Fixture(dut, (_, _, _) => (true, Seq(0x21)))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Byte(0x21))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX ack -> 10 byte -> P") { dut =>
    SimTimeout((100 ns) * 8 * 15)
    val response = Seq(0x11, 0x22, 0x33, 0x44, 0x55, 0x55, 0x77, 0x88, 0x99, 0xaa)
    val fixture = Fixture(dut, (_, _, _) => (true, response))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    response.foreach(i => scoreboard.pushRef(Byte(i)))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX ack -> 1 byte -> Sr -> ack -> P") { dut =>
    SimTimeout((100 ns) * 8 * 5)
    val fixture = Fixture(dut, (_, _, _) => (true, Seq(0x21)))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated=false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Byte(0x21))
    scoreboard.pushRef(Start(repeated=true))
    scoreboard.pushRef(Byte(0x54))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)
    dut.io.useRestart #= true

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false
    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    assert(!dut.io.ack.payload.toBoolean)

    dut.io.txData.fragment #= 0x54
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.useRestart #= false
    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX nack -> P") { dut =>
    SimTimeout((100 ns) * 8 * 5)
    val fixture = Fixture(dut, (_, _, _) => (false, Seq()))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX nack -> Sr -> nack -> P") { dut =>
    SimTimeout((100 ns) * 8 * 5)
    val fixture = Fixture(dut, (_, _, _) => (false, Seq()))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Start(repeated = true))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)
    dut.io.useRestart #= true

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.fragment.randomize()
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    assert(!dut.io.ack.payload.toBoolean)
    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.useRestart #= false

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    assert(!dut.io.ack.payload.toBoolean)

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX ack -> controller abort after 2 byte -> P") { dut =>
    SimTimeout((100 ns) * 8 * 5)
    val fixture = Fixture(dut, (_, _, _) => (true, Seq(0x21, 0x45, 0x67)))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Byte(0x21))
    scoreboard.pushRef(Byte(0x45))
    scoreboard.pushRef(Start(repeated = true))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.rxData.valid.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.rxData.valid.toBoolean)
    dut.io.continueRx #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA RX ack -> controller abort after 2 byte -> Sr -> ack -> 1 byte -> P") { dut =>
    SimTimeout((100 ns) * 8 * 10)
    val fixture = Fixture(dut, (_, _, _) => (true, Seq(0x21, 0x45, 0x67)))
    import fixture.scoreboard

    scoreboard.pushRef(Start(repeated = false))
    scoreboard.pushRef(Byte(0x55))
    scoreboard.pushRef(Byte(0x21))
    scoreboard.pushRef(Byte(0x45))
    scoreboard.pushRef(Start(repeated = true))
    scoreboard.pushRef(Byte(0x40))
    scoreboard.pushRef(Byte(0x41))
    scoreboard.pushRef(Stop())

    dut.io.txData.fragment #= 0x55
    dut.io.txData.valid #= true
    dut.io.txData.last #= true
    dut.io.useRestart #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.fragment.randomize()
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.rxData.valid.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.rxData.valid.toBoolean)
    dut.io.continueRx #= false

    dut.io.txData.fragment #= 0x40
    dut.io.txData.valid #= true
    dut.io.txData.last #= false
    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.useRestart #= false
    dut.io.txData.fragment #= 0x41
    dut.io.txData.last #= true
    dut.clockDomain.waitSamplingWhere(dut.io.txData.ready.toBoolean)
    dut.io.txData.fragment.randomize()
    dut.io.txData.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }
}
