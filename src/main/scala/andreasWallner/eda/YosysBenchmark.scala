package andreasWallner.eda

import spinal.core.{ClockDomainConfig, Component, Device, IClockDomainFrequency, LOW, SpinalConfig, UnknownFrequency}
import spinal.lib.WrapWithReg
import spinal.lib.eda.bench.Rtl
import spinal.core._

import java.io.File
import scala.collection.{Seq, mutable}
import scala.language.postfixOps

object Wrapper {
  def wrap(
      c: Component,
      collapseInput: Boolean,
      collapseOutput: Boolean,
      isolate: Boolean,
      combineWidth: Int = 4
  ): Unit = {
    var input = if (collapseInput) in(Bits(1 bit).setName("_input")) else null
    val allOutputs = mutable.ArrayBuffer[Bool]()
    for (e <- c.getOrdredNodeIo) {
      if (e.isInput) {
        val xx = if (isolate) {
          val inner = cloneOf(e)
          e := RegNext(RegNext(inner.setName(e.getName())))
          inner
        } else e

        if (collapseInput) {
          for (i <- 0 until xx.getBitsWidth) {
            e.assignFromBits(input, i, 1 bit)
            input = RegNext(input)
          }
        } else {
          xx.asInput()
        }
      } else {
        val xx = if (isolate) {
          RegNext(RegNext(e)).setName(e.getName())
        } else e

        if (collapseOutput) {
          val bits = xx.asBits
          for (i <- 0 until e.getBitsWidth) {
            allOutputs :+ bits(i)
          }
        }
      }
    }

    if (collapseOutput) {
      def makeBits(bs: Seq[Bool]): Bits = {
        val result = Bits(bs.size bit)
        bs.zipWithIndex.foreach { case (b, idx) => result(idx) := b }
        result
      }

      var allBits = makeBits(allOutputs)
      while (allBits.getBitsWidth > 1) allBits = makeBits(
        allBits.subdivideIn(combineWidth bit).map { s =>
          RegNext(s.xorR)
        }
      )

      allBits(0).asOutput().setName("_output")
    }
  }

  class Wrapper(
      c: => Component,
      collapseInput: Boolean,
      collapseOutput: Boolean,
      isolate: Boolean,
      combineWidth: Int = 4
  ) extends Component {
    val comp = c
    wrap(comp, collapseInput, collapseOutput, isolate, combineWidth)
  }
}

class YosysBenchmark(c: (String, () => Component)*) extends App {
  def defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency()
  def spinalConfig = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
    defaultClockDomainFrequency = defaultClockDomainFrequency,
    device = Device.LATTICE,
    targetDirectory = "benchmarkWorkspace"
  )

  val results = for ((id, cc) <- c) yield {
    new File("./benchmarkWorkspace").mkdirs()
    val cleanId = id.replace(' ', '_').replace('/', '_')
    val report = spinalConfig.generateVerilog {
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
  val line = "%1$-" + (maxIdLen + 2).toString + "s %2$-10s %3$-10s  "
  println(line.format("Name", "f max", "size"))
  println("-" * 40)
  for ((id, result) <- results) {
    val (engVal, unit) = result.fmax().decompose
    val relativeFMax = (10 * result.fmax().toBigDecimal / maxFMax + 0.5).toInt
    println(
      line.format(id, f"${engVal.toLong} $unit", result.getArea().split('\n').head) + ("*" * relativeFMax)
    )
  }
}


class VivadoBenchmark(c: (String, () => Component)*) extends App {
  def defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency()
  def spinalConfig = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
    defaultClockDomainFrequency = defaultClockDomainFrequency,
    device = Device.XILINX,
    targetDirectory = "benchmarkWorkspace"
  )

  val results = for ((id, cc) <- c) yield {
    new File("./benchmarkWorkspace").mkdirs()
    val cleanId = id.replace(' ', '_').replace('/', '_')
    val report = spinalConfig.generateVerilog {
      val wrapped = new WrapWithReg.Wrapper(cc())
      wrapped.setDefinitionName(wrapped.comp.definitionName + "__" + cleanId)
      wrapped
    }

    val synth = VivadoFlow(
      "",
      "benchmarkWorkspace",
      Rtl(report),
      family = "Artix 7",
      device = "xc7a200tffv1156-3",
      frequencyTarget=100 MHz,
      processorCount=4,
      verbose=true
    )
    (id, synth)
  }

  val maxIdLen = results.map(_._1.length).max
  val maxFMax = results.map(_._2.fmax().toBigDecimal).max
  val line = "%1$-" + (maxIdLen + 2).toString + "s %2$-10s %3$-10s  "
  println(line.format("Name", "f max", "size"))
  println("-" * 40)
  for ((id, result) <- results) {
    val (engVal, unit) = result.fmax().decompose
    val relativeFMax = (10 * result.fmax().toBigDecimal / maxFMax + 0.5).toInt
    println(
      line.format(id, f"${engVal.toLong} $unit", result.getArea().split('\n').head) + ("*" * relativeFMax)
    )
  }
}
