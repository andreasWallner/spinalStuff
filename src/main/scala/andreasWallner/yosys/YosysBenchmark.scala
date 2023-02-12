package andreasWallner.yosys

import spinal.core.{
  ClockDomainConfig,
  Component,
  Device,
  IClockDomainFrequency,
  LOW,
  SpinalConfig,
  UnknownFrequency
}
import spinal.lib.WrapWithReg
import spinal.lib.eda.bench.Rtl

class YosysBenchmark(c: (String, () => Component)*) extends App {
  def defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency()

  val results = for ((id, cc) <- c) yield {
    val cleanId = id.replace(' ', '_').replace('/', '_')
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      defaultClockDomainFrequency = defaultClockDomainFrequency,
      device = Device.LATTICE,
      targetDirectory = "benchmarkWorkspace"
    ).generateVerilog {
      val wrapped = new WrapWithReg.Wrapper(cc())
      wrapped.setDefinitionName(wrapped.comp.definitionName + "__" + cleanId)
      wrapped
    }

    val synth = YosysFlow(
      "benchmarkWorkspace",
      Rtl(report),
      family = "ice40",
      device = "lp1k",
      fpgaPackage = "qn84",
      allowUnconstrained = true,
      yosysPath = "/opt/oss-cad-suite-20230105/bin/",
      nextpnrPath = "/home/uhu01/git/nextpnr/",
      icestormPath = "/opt/oss-cad-suite-20230105/bin/"
    )
    (id, synth)
  }

  val line = "%1$-20s %2$-10s %3$-10s"
  println(line.format("Name", "f max", "size"))
  println("-" * 40)
  for ((id, result) <- results) {
    val (engVal, unit) = result.fmax().decompose
    println(line.format(id, f"${engVal.toLong} $unit", result.getArea().split('\n').head))
  }
}
