package andreasWallner.demos.icebreaker

//import andreasWallner.demos.icebreaker.Dot.report
import andreasWallner.eda.YosysFlow
import andreasWallner.eda.YosysFlow.dotDiagram
import andreasWallner.rtlutils._
import spinal.core._
import spinal.lib.eda.bench.Rtl
import andreasWallner.xilinx.InOutWrapper

import scala.language.postfixOps
import scala.tools.nsc.io.File

class iCEBreakerFlow[T <: Component](
    c: => T,
    workspace: String = "demo",
    forceDownload: Boolean = false,
    allowUnconstrained: Boolean = false,
    generateDot: Boolean = false,
    verbose: Boolean = false
) extends App {
  val report = SpinalConfig(
    targetDirectory = workspace,
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog { InOutWrapper(c, InOutWrapper.LatticeIce40SB_IO) }
  /*if (generateDot) {
    dotDiagram(workspace, Rtl(report))
  }*/
  val synth = YosysFlow(
    workspace,
    Rtl(report),
    "ice40",
    "up5k",
    "sg48",
    frequencyTarget = Some(report.toplevel.getFastestClock()),
    pcfFile = Some(File(".").toCanonical + "/extras/iCEBreaker.pcf"),
    allowUnconstrained = allowUnconstrained,
    verbose = verbose
  )
  val fmaxDec = synth.fmax().decompose
  println(f"${fmaxDec._1}%.1f ${fmaxDec._2}")
  println(synth.getArea())

  import scala.sys.process._
  if (args.contains("prog") || forceDownload)
    println(s"iceprog ${synth.bitstreamFile.get}" !!)
}
