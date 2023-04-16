package andreasWallner.io.i3c

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import andreasWallner.io.i3c.sim._
import andreasWallner.{LoggingScoreboardInOrder, SpinalFunSuite}
import andreasWallner.sim._
import org.scalactic.TimesOnInt.convertIntToRepeater

import scala.language.postfixOps
import scala.util.Random

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
    txScoreboard.pushRef(Byte(0x76))
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

    dut.io.trigger #= true
    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.data #= 0x76
    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.trigger #= false
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    sleep(100)

    txScoreboard.checkEmptyness()
  }

  test(dut, "SDA TX ack -> 100 byte -> S") { dut =>
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

    val sequence = Seq(0x54) ++ Seq.fill(100){Random.nextInt(256)}

    val txScoreboard = ScoreboardInOrder[Event]()
    txScoreboard.pushRef(Start(repeated = false))
    sequence.foreach(i => txScoreboard.pushRef(Byte(i)))
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

    for (i <- sequence) {
      dut.io.data #= i
      dut.io.trigger #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    }
    dut.io.trigger #= false
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    sleep(100)

    txScoreboard.checkEmptyness()
  }

  test(dut, "SDA TX ack -> 20 * (x byte -> R|S)", -1082496406) { dut =>
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

    val txScoreboard = LoggingScoreboardInOrder[Event]()

    dut.clockDomain.forkStimulus((100 MHz).toTime)
    dut.clockDomain.waitSampling()
    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = (true, Seq())

      override def event(e: Event): Unit = {
        e match {
          case Bit(_) =>
          case _ =>//simLog(e)
        }
        e match {
          case Byte(_) | Start(_) | Stop() => txScoreboard.pushDut(e)
          case Bit(_) =>
          case _ => fail("sim target saw unexpected event")
        }
      }
    }

    var useRepeatStart = false
    for(i <- 0 until 20) {
      txScoreboard.pushRef(Start(dut.io.useRestart.toBoolean))

      val n = Random.nextInt(20)
      dut.io.useRestart #= useRepeatStart
      useRepeatStart = Random.nextBoolean() & (i != 18)

      val address = Random.nextInt(256) & ~0x01 // ensure write transfer
      simLog(address)
      txScoreboard.pushRef(Byte(address))
      dut.io.data #= address
      dut.io.trigger #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)

      for (i <- 0 until n) {
        val byte = dut.io.data.randomize().toInt
        txScoreboard.pushRef(Byte(byte))
        dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
      }
      dut.io.trigger #= false
      if(dut.io.useRestart.toBoolean)
        dut.clockDomain.waitSamplingWhere(dut.io.rStart.toBoolean)
      else
        dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

      if (!dut.io.useRestart.toBoolean)
        txScoreboard.pushRef(Stop())
    }

    sleep(100)

    txScoreboard.checkEmptyness()
  }
}
