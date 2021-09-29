package andreasWallner

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import andreasWallner.basics.{AxiLite4Pwm, PwmGenerics}
import andreasWallner._
import andreasWallner.xilinx._
import andreasWallner.io.pwm._
import andreasWallner.registers.generator.{CHeader, YAML, CppHeader, KvasirHeader}
import andreasWallner.registers.datamodel.BusComponent

object AxiLite4PwmTopLevel {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(XilinxNamer(AxiLite4Pwm(PwmGenerics(8, 3))))
  }
}

object AxiLite4PwmModule {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(AxiLite4Pwm(PwmGenerics(8, 3)))
    VivadoIpify.emitComponentXml(report.toplevel)
  }
}

object Apb3PwmModule {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(
      Apb3Pwm(
        Pwm.PeripheralParameters(Pwm.CoreParameters(channelCnt = 5))
      )
    );
  }
}

object ApbSpinalTap {
  def main(args: Array[String]): Unit = {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(100 MHz),
      device = Device.XILINX,
      targetDirectory = "generated"
    ).generateVerilog(
      XilinxNamer(XilinxInOutWrapper(new andreasWallner.spinaltap.ApbSpinalTap()))
    );
    for (e <- report.toplevel.elements) {
      e match {
        case (b: BusComponent, offset: Long) =>
          new CHeader(b).write()
          new CppHeader(b, namespace=Some(s"spinaltap::${b.busComponentName.toLowerCase}::registers")).write()
          new KvasirHeader(b, namespace=Some(s"spinaltap::register::${b.busComponentName.toLowerCase}")).write()
          new YAML(b).write()
        case other => println("Not generating for " + other)
      }
    }
  }
}
