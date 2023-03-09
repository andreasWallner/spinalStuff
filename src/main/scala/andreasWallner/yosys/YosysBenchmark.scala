package andreasWallner.yosys

import spinal.core.{ClockDomainConfig, Component, Device, IClockDomainFrequency, LOW, SpinalConfig, UnknownFrequency}
import spinal.lib.WrapWithReg
import spinal.lib.eda.bench.Rtl

import java.io.File

class YosysBenchmark(c: (String, () => Component)*) extends App {
  def defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency()

  val results = for ((id, cc) <- c) yield {
    new File("./benchmarkWorkspace").mkdirs()
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
      device = "hx8k",
      fpgaPackage = "ct256",
      allowUnconstrained = true,
      yosysPath = "/opt/oss-cad-suite-20230105/bin/",
      nextpnrPath = "/home/uhu01/git/nextpnr/",
      icestormPath = "/opt/oss-cad-suite-20230105/bin/"
    )
    (id, synth)
  }

  val maxIdLen = results.map(_._1.length).max
  val maxFMax = results.map(_._2.fmax().toBigDecimal).max
  val line = "%1$-" + (maxIdLen+2).toString + "s %2$-10s %3$-10s  "
  println(line.format("Name", "f max", "size"))
  println("-" * 40)
  for ((id, result) <- results) {
    val (engVal, unit) = result.fmax().decompose
    val relativeFMax = (10 * result.fmax().toBigDecimal / maxFMax + 0.5).toInt
    println(line.format(id, f"${engVal.toLong} $unit", result.getArea().split('\n').head) + ("*" * relativeFMax))
  }
}
import scala.math.Ordering.Implicits
