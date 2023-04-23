package andreasWallner

import andreasWallner.basics.{AxiLite4Pwm, PwmGenerics}
import andreasWallner.io.pwm._
import andreasWallner.registers.datamodel.BusComponent
import andreasWallner.registers.generator.{CHeader, CppHeader, HTML, YAML}
import andreasWallner.spinaltap.MuxConnections
import andreasWallner.xilinx._
import spinal.core._

import scala.language.postfixOps

object AxiLite4PwmTopLevel extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
    device = Device.XILINX
  ).generateVerilog(XilinxNamer(AxiLite4Pwm(PwmGenerics(8, 3))))
}

object AxiLite4PwmModule extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
    device = Device.XILINX
  ).generateVerilog(AxiLite4Pwm(PwmGenerics(8, 3)))
  VivadoIpify.emitComponentXml(report.toplevel)
}

object Apb3PwmModule extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
    device = Device.XILINX
  ).generateVerilog(
    Apb3Pwm(
      Pwm.PeripheralParameters(Pwm.CoreParameters(channelCnt = 5))
    )
  )
}

object ApbSpinalTap extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
    defaultClockDomainFrequency = FixedFrequency(100 MHz),
    device = Device.XILINX,
    targetDirectory = "generated"
  ).generateVerilog(
    XilinxNamer(XilinxInOutWrapper(new andreasWallner.spinaltap.ApbSpinalTap()))
  )
  new MuxConnections.CppHeader(report.toplevel.muxConnections, Some("spinaltap::iomux"))
    .write(f"${GlobalData.get.phaseContext.config.targetDirectory}/iomux_connection.hpp")
  for (e <- report.toplevel.elements) {
    e match {
      case (b: BusComponent, offset: Long) =>
        println(f"${b.busComponentName} @ $offset%x")
        new CHeader(b).write()
        new CppHeader(
          b,
          namespace = Some(s"spinaltap::${b.busComponentName.toLowerCase}::registers")
        ).write()
        new YAML(b).write()
        new HTML(b).write()
      case other => println("Not generating for " + other)
    }
  }
}
