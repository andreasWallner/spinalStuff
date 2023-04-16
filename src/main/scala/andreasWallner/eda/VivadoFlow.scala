package andreasWallner.eda

import org.apache.commons.io.FileUtils
import spinal.core.HertzNumber
import spinal.lib.DoCmd.doCmd
import spinal.lib.eda.bench.{Report, Rtl}

import java.io.File
import java.nio.file.Paths
import scala.io.Source
import scala.sys.process.{Process, ProcessIO}
import spinal.core._

import scala.collection.mutable
import scala.language.postfixOps

trait VivadoSynthesisReport extends Report {
  def fmax(clk: String = null): HertzNumber
  def resources: Map[String, (Int, Int)]
}

object VivadoFlow {

  /**
    * Use vivado to run eda flow
    *
    * @param vivadoPath      The path to vivado (e.g. /opt/Xilinx/Vivado/2019.2/bin)
    * @param workspacePath   The temporary workspace path (e.g. /tmp/test)
    * @param toplevelPath    The path to top level hdl file
    * @param family          Xilinx device family (Artix 7, Kintex 7, Kintex UltraScale, Kintex UltraScale+ or Virtex UltraScale+)
    * @param device          Xilinx device part
    * @param frequencyTarget Target clock frequency
    * @param processorCount  Number of processor count used
    * @return Report
    */
  def apply(
      vivadoPath: String = "",
      workspacePath: String,
      rtl: Rtl,
      family: String="Artix 7",
      device: String="xc7a200tffv1156-3",
      frequencyTarget: HertzNumber = null,
      processorCount: Int = 1,
      additionalXdc: Seq[String] = List(),
      verbose: Boolean = false
  ): VivadoSynthesisReport = {
    assert(vivadoPath == "" || vivadoPath.endsWith("/"), "vivadoPath must end with a '/' if set")

    val targetPeriod = (if (frequencyTarget != null) frequencyTarget else (400 MHz)).toTime

    val workspacePathFile = new File(workspacePath)
    workspacePathFile.mkdir()

    val isVhdl = (file: String) => file.endsWith(".vhd") || file.endsWith(".vhdl")
    val xdcName = f"clk_${targetPeriod}.xdc"
    if (!new File(xdcName).exists()) {
      val xdc = new java.io.FileWriter(Paths.get(workspacePath, xdcName).toFile)
      xdc.write(s"""create_clock -period ${(targetPeriod * 1e9).toBigDecimal} [get_ports clk]""")
      xdc.close();
    }

    // generate tcl script
    val tcl = mutable.ArrayBuffer[String]() ++: rtl
      .getRtlPaths()
      .map(file =>
        s"""read_${if (isVhdl(file)) "vhdl" else "verilog"} ${Paths.get(file).toAbsolutePath}"""
      ) ++: additionalXdc.map(file => s"read_xdc $file") ++: Seq(
      f"read_xdc $xdcName",
      f"synth_design -part $device -top ${rtl.getTopModuleName()}",
      "opt_design",
      "place_design",
      "route_design",
      "report_utilization",
      "report_timing",
      "exit"
    )

    val vivadoProcess = Process(
      Seq(f"${vivadoPath}vivado", "-nojournal", "-mode", "tcl"),
      new java.io.File(workspacePath)
    )
    val output = new mutable.StringBuilder
    val line = new mutable.StringBuilder
    val errline = new mutable.StringBuilder
    val vivado = new ProcessIO(
      in => {
        tcl.foreach { line =>
          in.write(line.getBytes("UTF-8"))
          in.write("\n".getBytes("UTF-8"))
        }
        in.close()
      },
      out => {
        val src = io.Source.fromInputStream(out)
        src.foreach { l =>
          output.append(l)
          line.append(l)
          if (l == '\n') {
            print(line.toString())
            line.clear()
          }
        }
        src.close()
      },
      err => {
        val src = io.Source.fromInputStream(err)
        src.foreach { l =>
          output.append(l)
          errline.append(l)
          if (l == '\n') {
            print(errline.toString())
            errline.clear()
          }
        }
      }
    )
    val exitCode = vivadoProcess.run(vivado).exitValue()
    println(exitCode)

    val report = output.mkString

    new VivadoSynthesisReport {
      override def fmax(clk: String = null) = HertzNumber(getFMax())
      override def getFMax(): Double = {
        val intFind = "-?(\\d+\\.?)+".r
        val slack =
          try {
            (family match {
              case "Artix 7" | "Kintex 7" | "Kintex UltraScale" | "Kintex UltraScale+" |
                  "Virtex UltraScale+" =>
                intFind
                  .findFirstIn(
                    "-?(\\d+.?)+ns  \\(required time - arrival time\\)".r.findFirstIn(report).get
                  )
                  .get
            }).toDouble
          } catch {
            case e: Exception => -100000.0
          }
        return 1.0 / (targetPeriod.toDouble - slack * 1e-9)
      }
      override def getArea(): String = {
        val intFind = "(\\d+,?)+".r
        val leArea =
          try {
            family match {
              case "Artix 7" | "Kintex 7" =>
                intFind
                  .findFirstIn("Slice LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get)
                  .get + " LUT " +
                  intFind
                    .findFirstIn("Slice Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get)
                    .get + " FF "
              case "Kintex UltraScale" | "Kintex UltraScale+" | "Virtex UltraScale+" =>
                intFind
                  .findFirstIn("CLB LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get)
                  .get + " LUT " +
                  intFind
                    .findFirstIn("CLB Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get)
                    .get + " FF "
            }
          } catch {
            case e: Exception => "???"
          }
        return leArea
      }

      override def resources = {
        null
      }
    }
  }
}
