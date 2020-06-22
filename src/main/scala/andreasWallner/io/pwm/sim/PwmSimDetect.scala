package andreasWallner.io.pwm.sim

import spinal.core._
import spinal.sim._
import spinal.core.sim._

sealed trait PwmResult
case class PwmConstant(state: Boolean) extends PwmResult
case class PwmCycle(high: Long, low: Long) extends PwmResult

case class PwmDetect(pwm: Bool, period_limit: Long, clockDomain: ClockDomain)(
    report: (PwmResult) => Unit
) {
  var limit = period_limit
  var active = false
  var lastState = pwm.toBoolean

  var risingEdge: Option[Long] = None
  var fallingEdge: Option[Long] = None
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
        if (risingEdge.isDefined) {
          report(
            PwmCycle(
              fallingEdge.get - risingEdge.get,
              simTime() - fallingEdge.get
            )
          )
        }
        risingEdge = Option(simTime())
      } else {
        fallingEdge = Option(simTime())
      }
    }
    lastState = pwm.toBoolean
  }
  clockDomain.onSamplings(fsm)
}
