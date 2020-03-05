package innovative_solutions

import spinal.core._
import innovative_solutions.basics.{AxiLite4Pwm, PwmGenerics}

object AxiLite4PwmTopLevel {
  def main(args: Array[String]) {
    val report = SpinalConfig(defaultConfigForClockDomains =
      ClockDomainConfig(resetActiveLevel = LOW)
    ).generateVerilog(XilinxNamer.name(AxiLite4Pwm(PwmGenerics(8, 3))))
  }
}
