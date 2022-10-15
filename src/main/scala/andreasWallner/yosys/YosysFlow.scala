package andreasWallner.yosys

import spinal.core.HertzNumber
import spinal.lib.DoCmd.doCmd
import spinal.lib.eda.bench.{Report, Rtl}

import java.nio.file.Paths
import scala.language.postfixOps
import scala.sys.process.Process
import scala.util.parsing.json.JSON

object YosysFlow {

  /**
    * family: ice40, ecp5
    * device: lp1k
    * fpgaPackage: qn84
    */
  def apply(
      yosysPath: String,
      workspacePath: String,
      rtl: Rtl,
      family: String,
      device: String,
      fpgaPackage: String,
      frequencyTarget: Option[HertzNumber] = None,
      pcfFile: Option[String] = None
  ): Report = {
    def doCmd(cmd:Seq[String], path:String):Unit = {
      val process = Process(cmd, new java.io.File(path))
      println(process)
      process!
    }

    val frequencyMHz =
      frequencyTarget.map(x => x.toDouble / 1e6).getOrElse(100.0)
    val isVhdl = (file: String) =>
      file.endsWith(".vhd") || file.endsWith(".vhdl")
    if (rtl.getRtlPaths().exists(file => isVhdl(file)))
      throw new RuntimeException("Yosys flow can only run with verilog sources")

    if (family != "ice40")
      throw new RuntimeException(
        "Currently only support for ice40 is implemented"
      )
    val readRtl =
      rtl.getRtlPaths().map(file => s"${Paths.get(file).toAbsolutePath}")
    doCmd(
      Seq(
        yosysPath,
        "-l",
        "yosys.log",
        "-p",
        s"synth_ice40 -top ${rtl.getTopModuleName()} -json ${rtl.getName()}.json"
      )
        ++ readRtl,
      workspacePath
    )

    doCmd(
      Seq(
        "nextpnr-ice40",
        "--freq",
        s"$frequencyMHz",
        s"--$device",
        "--package",
        fpgaPackage,
        "--json",
        s"${rtl.getName()}.json",
        "--asc",
        s"${rtl.getName()}.asc",
        "--report",
        s"${rtl.getName()}_report.json"
      )
        ++ pcfFile
          .map(x => Seq("--pcf", x))
          .getOrElse(Seq("--pcf-allow-unconstrained")),
      workspacePath
    )

    doCmd(
      Seq("icetime", "-d", device)
        ++ pcfFile.map(x => Seq("-p", x)).getOrElse(Seq())
        ++ Seq("-c", s"${frequencyMHz}MHz")
        ++ Seq("-mtr", s"${rtl.getName()}.rpt", s"${rtl.getName()}.asc"),
      workspacePath
    )
    if(pcfFile.isDefined)
      doCmd(
        Seq("icepack", s"${rtl.getName()}.asc", s"${rtl.getName()}.bin"),
        workspacePath
      )

    val source = io.Source.fromFile(
      new java.io.File(workspacePath, s"${rtl.getName()}_report.json")
    )
    val json =
      try JSON.parseFull(source.mkString)
      finally source.close()

    // thanks to huynhjl (https://stackoverflow.com/a/4186090) for the idea with unapply
    class CC[T] {
      def unapply(a: Option[Any]): Option[T] = a.map(aa => aa.asInstanceOf[T])
    }

    object M extends CC[Map[String, Any]]
    object D extends CC[Double]

    new Report {
      override def getFMax(): Double = {
        try {
          val M(root) = json
          val M(fmax) = root.get("fmax")
          val M(clk) = fmax.get(fmax.keys.last)
          val D(achieved) = clk.get("achieved")
          achieved
        } catch {
          case _: Exception => 0.0
        }
      }
      override def getArea(): String = {
        try {
          val M(root) = json
          val M(utilization) = root.get("utilization")
          val M(lc) = utilization.get("ICESTORM_LC")
          val M(ram) = utilization.get("ICESTORM_RAM")
          val D(lc_used) = lc.get("used")
          val D(lc_available) = lc.get("available")
          val D(ram_used) = ram.get("used")
          val D(ram_available) = ram.get("available")
          lc_used.toLong.toString + "/" + lc_available.toLong
            .toString + " LCs\n" +
            ram_used.toLong.toString + "/" + ram_available.toLong
            .toString + " RAMs"
        } catch {
          case _: Exception => "???"
        }
      }
    }
  }
}