package andreasWallner

import spinal.core._
import andreasWallner.basics.{AxiLite4Pwm, PwmGenerics}
import andreasWallner._
import andreasWallner.xilinx._

object AxiLite4PwmTopLevel {
  def main(args: Array[String]) {
    val report = SpinalConfig(defaultConfigForClockDomains =
      ClockDomainConfig(resetActiveLevel = LOW),
      device=Device.XILINX
    ).generateVerilog(XilinxNamer(AxiLite4Pwm(PwmGenerics(8, 3))))
  }
}

object AxiLite4PwmModule {
  def main(args: Array[String]) {
    val report = SpinalConfig(defaultConfigForClockDomains =
      ClockDomainConfig(resetActiveLevel = LOW),
      device=Device.XILINX
    ).generateVerilog(AxiLite4Pwm(PwmGenerics(8,3)))
    VivadoIpify.emitComponentXml(report.toplevel)
  }
}

object HsiComponent {
  def main(args: Array[String]) {
    val report = SpinalConfig(defaultConfigForClockDomains =
      ClockDomainConfig(resetActiveLevel = LOW),
      device=Device.XILINX
    ).generateVerilog(XilinxNamer(ztex.HsiInterface()))
  }
}
