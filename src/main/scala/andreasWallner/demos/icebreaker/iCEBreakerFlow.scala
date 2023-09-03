package andreasWallner.demos.icebreaker

import andreasWallner.eda.YosysFlow
import andreasWallner.rtlutils._
import spinal.core._
import spinal.lib.eda.bench.Rtl
import spinal.lib.io.InOutWrapper

import scala.language.postfixOps
import scala.tools.nsc.io.File

class iCEBreakerFlow[T <: Component](
    c: => T,
    workspace: String = "demo",
    forceDownload: Boolean = false
) extends App {
  val report = SpinalConfig(
    targetDirectory = workspace,
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog { InOutWrapper(c) }
  val synth = YosysFlow(
    workspace,
    Rtl(report),
    "ice40",
    "up5k",
    "sg48",
    frequencyTarget = Some(report.toplevel.getFastestClock()),
    pcfFile = Some(File(".").toCanonical + "/extras/iCEBreaker.pcf"),

  )
  val fmaxDec = synth.fmax().decompose
  println(f"${fmaxDec._1}%.1f ${fmaxDec._2}")
  println(synth.getArea())

  import scala.sys.process._
  if (args.contains("prog") || forceDownload)
    println(s"iceprog ${synth.bitstreamFile.get}" !!)
}
