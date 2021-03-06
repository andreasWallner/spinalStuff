package andreasWallner.basics

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.collection.mutable.Queue

sealed trait PwmResult
case class PwmConstant(state: Boolean) extends PwmResult
case class PwmCycle(high: Long, low: Long) extends PwmResult

case class PwmDetect(pwm: Bool, period_limit: Long, clockDomain: ClockDomain)(
    report: (PwmResult) => Unit
) {
  var lastState = pwm.toBoolean
  
  var limit = period_limit
  var active = false
  
  var risingEdge: java.lang.Long = null
  var fallingEdge: java.lang.Long = null

  def fsm(): Unit = {
    limit = limit - 1
    if (limit == 0) {
      report(PwmConstant(pwm.toBoolean))
      limit = period_limit
    }

    if (!active) {
      active = !pwm.toBoolean
    } else if (pwm.toBoolean != lastState) {
      limit = period_limit
      if (pwm.toBoolean) { // rising edge
        if (risingEdge != null) {
          report(PwmCycle(fallingEdge - risingEdge, simTime() - fallingEdge))
        }
        risingEdge = simTime()
      } else {
        fallingEdge = simTime()
      }
    }
    lastState = pwm.toBoolean
  }
  clockDomain.onSamplings(fsm)

  def clear() = {
    limit = period_limit
    active = false
    lastState = pwm.toBoolean
    risingEdge = null
    fallingEdge = null
  }
}

object PwmSim {
  def main(args: Array[String]) {
    val dut = SimConfig.withWave
      .workspacePath("/mnt/c/work/tmp/sim")
      .compile(Pwm(PwmGenerics(3, 1)))

    dut.doSim("constant levelss") { dut =>
      SimTimeout(200 * 10)
      val pwm_times = new Queue[PwmResult]
      val decoder = PwmDetect(dut.io.pwms(0), 10, dut.clockDomain) { result =>
        println(f"${simTime()}: ${result}")
        pwm_times += result
      }
      dut.io.levels(0) #= 0
      dut.io.max_count #= 7
      dut.clockDomain.forkStimulus(period = 10)

      // if level is zero, we do not want any output
      // verify for a few "detections"
      dut.clockDomain.waitRisingEdgeWhere(pwm_times.size >= 3)
      assert(pwm_times.forall(_ == PwmConstant(false)) == true)

      pwm_times.clear()
      decoder.clear()

      dut.io.levels(0) #= 1
      dut.clockDomain.waitRisingEdgeWhere(pwm_times.size >= 3)
      assert(pwm_times.forall(_ == PwmCycle(10, 70)) == true)

      pwm_times.clear()
      decoder.clear()

      dut.io.max_count #= 6
      dut.io.levels(0) #= 7
      dut.clockDomain.waitRisingEdgeWhere(pwm_times.size >= 3)
      assert(pwm_times.forall(_ == PwmConstant(true)) == true)
    }

    dut.doSim("levels change") { dut =>
      SimTimeout(200 * 10)
      val pwm_times = new Queue[PwmResult]
      val decoder = PwmDetect(dut.io.pwms(0), 10, dut.clockDomain) { result =>
        println(f"${simTime()}: ${result}")
        pwm_times += result
      }
      dut.io.levels(0) #= 0
      dut.io.max_count #= 6
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge(9)
      dut.io.levels(0) #= 6
      dut.clockDomain.waitRisingEdge(30)
      assert(pwm_times.forall(_ match {
        case PwmCycle(high, low) => high + low == 70
        case PwmConstant(false)  => true
        case PwmConstant(true) => false
      }))
    }
  }
}
