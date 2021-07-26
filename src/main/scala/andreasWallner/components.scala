package andreasWallner

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import andreasWallner.basics.{AxiLite4Pwm, PwmGenerics}
import andreasWallner._
import andreasWallner.xilinx._
import andreasWallner.io.pwm._
//import spinal.lib.bus.regif.Document.HtmlGenerator
import andreasWallner.io.iso7816._

import spinal.lib.bus.regif._
import spinal.lib.bus.regif.generator._

object AxiLite4PwmTopLevel {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      device = Device.XILINX
    ).generateVerilog(XilinxNamer(AxiLite4Pwm(PwmGenerics(8, 3))))
  }
}

object AxiLite4PwmModule {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      device = Device.XILINX
    ).generateVerilog(XilinxNamer(AxiLite4Pwm(PwmGenerics(8, 3))))
    VivadoIpify.emitComponentXml(report.toplevel)
  }
}

object Apb3PwmModule {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      device = Device.XILINX
    ).generateVerilog(
      Apb3Pwm(
        Pwm.PeripheralParameters(Pwm.CoreParameters(channelCnt = 5))
      )
    );
  }
}

object AxiLite4ISO7816Module {
  def main(args: Array[String]): Unit = {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(100 MHz),
      device = Device.XILINX
    ).generateVerilog(
      XilinxNamer(XilinxInOutWrapper(AxiLite4ISO7816Peripheral()), noClockSpeed=true)
    );
  }
}

object Ahb3PwmTest {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      device = Device.XILINX
    ).generateVerilog(
      new RegIfAhbLite3Pwm(Pwm.PeripheralParameters(coreParameters=Pwm.CoreParameters(channelCnt=10)))
    )
    new CHeader(report.toplevel.registerBanks()(0)).overrideName("PWM").write()
    new HTML(report.toplevel.registerBanks()(0)).overrideName("PWM").write()
    new DetailedHTML(report.toplevel.registerBanks()(0)).overrideName("DetailedPWM").write()
  }
}
