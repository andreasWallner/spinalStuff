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
  case class Fixture(dut: SdaTx, sendAck: Boolean) {
    val txScoreboard = ScoreboardInOrder[Event]()

    dut.io.tCas #= 2
    dut.io.changeCnt #= 1
    dut.io.lowCnt #= 7
    dut.io.bitCnt #= 8
    dut.io.stopCnt #= 10
    dut.io.trigger #= false
    dut.io.useRestart #= false

    dut.io.i3c.sda.simulatePullup()
    dut.io.i3c.scl.simulatePullup()

    // fork and wait before setting up I3CSimTarget to avoid it seeing non-reset random values
    dut.clockDomain.forkStimulus((100 MHz).toTime)
    dut.clockDomain.waitSampling()

    new I3CSimTarget(dut.io.i3c, dut.clockDomain) {
      override def addressReaction(address: Int, RnW: Boolean, repeated: Boolean) = (sendAck, Seq())

      override def event(e: Event): Unit = {
        e match {
          case Bit(_) =>
          case _ => //simLog(e)
        }
        e match {
          case Byte(_) | Start(_) | Stop() => txScoreboard.pushDut(e)
          case Bit(_) =>
          case _ => fail("sim target saw unexpected event")
        }
      }
    }

    def finish(): Unit = {
      sleep(100)
      txScoreboard.checkEmptyness()
    }
  }

  val simString = SimString("debug_msg")
  val dut = SimConfig.withFstWave.compile { SdaTx().add(simString) }

  // test simple behavior when seeing NACK after addressing, generating stop
  test(dut, "SDA TX nack -> S") { dut =>
    val fixture = Fixture(dut, sendAck=false)
    import fixture.txScoreboard

    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Stop())

    dut.io.data.fragment #= 0x54
    dut.io.data.valid #= true
    dut.io.data.last #= true
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    dut.io.data.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  // test simple behavior when seeing NACK after addressing, generate repeated start
  // set up signals as if we wanted to send multiple bytes, and have to switch
  // to next address in time for repeated start (w/o follow-up data)
  test(dut, "SDA TX nack -> R") { dut =>
    val fixture = Fixture(dut, sendAck=false)
    import fixture.txScoreboard

    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Start(repeated = true))
    txScoreboard.pushRef(Byte(0x43))
    txScoreboard.pushRef(Stop())

    dut.io.useRestart #= true

    dut.io.data.fragment #= 0x54
    dut.io.data.last #= false
    dut.io.data.valid #= true
    dut.io.trigger.strobe(dut.clockDomain)
    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)

    // assign data to be sent as control SM would, keep last == 0
    dut.io.data.fragment #= 0x77

    // wait for NACK to be reported
    dut.clockDomain.waitSamplingWhere(dut.io.ack.valid.toBoolean)
    assert(!dut.io.ack.payload.toBoolean)

    // assign new address
    dut.io.data.fragment #= 0x43
    dut.io.data.last #= true
    dut.io.useRestart #= false
    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    dut.io.data.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA TX ack -> 1 byte -> S") { dut =>
    val fixture = Fixture(dut, sendAck=true)
    import fixture.txScoreboard

    txScoreboard.pushRef(Start(repeated = false))
    txScoreboard.pushRef(Byte(0x54))
    txScoreboard.pushRef(Byte(0x76))
    txScoreboard.pushRef(Stop())

    dut.io.data.fragment #= 0x54
    dut.io.data.valid #= true
    dut.io.data.last #= false
    dut.io.trigger.strobe(dut.clockDomain)

    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    dut.io.data.fragment #= 0x76
    dut.io.data.last #= true

    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    dut.io.data.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA TX ack -> 100 byte -> S") { dut =>
    val fixture = Fixture(dut, sendAck=true)
    import fixture.txScoreboard

    val sequence = Seq(0x54) ++ Seq.fill(100){Random.nextInt(256)}

    txScoreboard.pushRef(Start(repeated = false))
    sequence.foreach(i => txScoreboard.pushRef(Byte(i)))
    txScoreboard.pushRef(Stop())
    dut.clockDomain.waitSampling()

    for (i <- sequence.dropRight(1)) {
      dut.io.data.fragment #= i
      dut.io.data.valid #= true
      dut.io.data.last #= false
      dut.io.trigger.strobe(dut.clockDomain) // TODO only do o first
      dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    }
    dut.io.data.fragment #= sequence.last
    dut.io.data.last #= true
    dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
    dut.io.data.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)

    fixture.finish()
  }

  test(dut, "SDA TX ack -> 20 * (x byte -> R|S)") { dut =>
    val fixture = Fixture(dut, sendAck=true)
    import fixture.txScoreboard

    var useRepeatStartNext = false
    for(i <- 0 until 20) {
      txScoreboard.pushRef(Start(dut.io.useRestart.toBoolean))

      val n = Random.nextInt(20)
      dut.io.useRestart #= useRepeatStartNext
      useRepeatStartNext = Random.nextBoolean() & (i != 18)

      val address = Random.nextInt(256) & ~0x01 // ensure write transfer
      txScoreboard.pushRef(Byte(address))
      dut.io.data.fragment #= address
      dut.io.data.last #= n == 0
      dut.io.data.valid #= true
      dut.io.trigger #= true
      dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
      dut.io.trigger #= false
      dut.io.data.valid #= false

      for (i <- 0 until n) {
        val byte = dut.io.data.fragment.randomize().toInt
        txScoreboard.pushRef(Byte(byte))
        dut.io.data.valid #= true
        dut.io.data.last #= i == (n-1)
        dut.clockDomain.waitSamplingWhere(dut.io.data.ready.toBoolean)
        dut.io.data.valid #= false
      }
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
