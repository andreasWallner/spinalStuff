package andreasWallner

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import andreasWallner.basics.{AxiLite4Pwm, PwmGenerics}
import andreasWallner._
import andreasWallner.xilinx._
import andreasWallner.io.pwm._
import andreasWallner.registers.generator.{CHeader, YAML, CppHeader, KvasirHeader}
import andreasWallner.spinaltap.MuxConnections
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

object Ulx3sRenamer {
  def apply[T <: Component](c: T): T = {
    c.addPrePopTask(() => {
      c.clockDomain.clock.setName("clk_25mhz")
    })
    c
  }
}

case class Ulx3s() extends Component {
  val io = new Bundle {
    val clk_25mhz = in Bool()
    val led = out Bits(8 bit)
    /* ... */
  }
  noIoPrefix()
  val clock_domain = ClockDomain(
    clock=io.clk_25mhz,
    frequency=FixedFrequency(25 MHz),
    config=ClockDomainConfig(resetKind=BOOT)
  )
  new ClockingArea(clock_domain) {
    /* place all your logic here e.g. */
    val cntr = Reg(UInt(8 bit)) init 0
    val slower = new SlowArea(1 Hz) {
      cntr := cntr + 1
    }
    io.led := cntr.asBits
  }
}

object Foo {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateVerilog(Ulx3s())
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
      /*XilinxNamer(XilinxInOutWrapper*/Ulx3sRenamer(new andreasWallner.spinaltap.ApbSpinalTap())/*)*/
    );

    new MuxConnections.CppHeader(report.toplevel.muxConnections, Some("spinaltap::iomux")).write(f"${GlobalData.get.phaseContext.config.targetDirectory}/iomux_connection.hpp")
    for (e <- report.toplevel.elements) {
      e match {
        case (b: BusComponent, offset: Long) =>
          new CHeader(b).write()
          new CppHeader(b, namespace=Some(s"spinaltap::${b.busComponentName.toLowerCase}::registers")).write()
          //new KvasirHeader(b, namespace=Some(s"spinaltap::register::${b.busComponentName.toLowerCase}")).write()
          new YAML(b).write()
        case other => println("Not generating for " + other)
      }
    }
  }
}
