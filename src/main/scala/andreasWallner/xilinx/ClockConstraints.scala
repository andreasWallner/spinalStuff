package andreasWallner.xilinx

import andreasWallner.Utils.DumpAST
import andreasWallner.eda.VivadoFlow
import spinal.core._
import spinal.core.internals._
import spinal.lib.eda.bench.Rtl
import spinal.lib.tools.ModuleAnalyzer

import java.io.{File, PrintWriter, Writer}
import scala.collection.mutable
import scala.language.postfixOps

object crossClockFalsePath extends SpinalTag
class crossClockMaxDelay(cycles: Int, useTargetClock: Boolean) extends SpinalTag {
  override def allowMultipleInstance = false
}
// TODO alternative constructors? (clockDomain & factor of period)
class crossClockMaxBusSkew(value: TimeNumber) extends SpinalTag {
  override def allowMultipleInstance = false
}

object ConstraintWriter {
  def apply[T <: Component](
      c: T,
      filename: String = null,
      trustSpinalTimings: Boolean = false
  ): T = {
    val realFilename = Option(filename).getOrElse(
      f"${GlobalData.get.phaseContext.config.targetDirectory}/${c.getClass.getSimpleName}.tcl"
    )
    val writer = new PrintWriter(new File(realFilename))
    c.walkComponents(cc => cc.dslBody.walkStatements(doWalkStatements(_, writer)))
    writer.close()
    c
  }

  def doWalkStatements(s: Statement, writer: Writer): Unit = {
    s match {
      case da: DataAssignmentStatement =>
        da.target match {
          case str: SpinalTagReady if str.hasTag(crossClockFalsePath) => writeFalsePathFromTo(da, writer)
          case str: SpinalTagReady if str.hasTag(classOf[crossClockMaxDelay]) =>
            writeMaxDelay(da, str.getTag(classOf[crossClockMaxDelay]).get, writer)
          case _ =>
        }
      case _ =>
    }
  }

  def portOrPinSelector(it: BaseType, traceOutput: Boolean=true): String = {
    val busPostfix = if(it.getBitsWidth > 1) "[*]" else ""
    // It seems we need to use get_pin if it's a pin on the toplevel or a component
    // and get_port to get the ports of e.g. a flip-flop (in general any connection).
    // Since we trace to registers we can only get an IO if it's on the toplevel
    if(it.isInOut) {
      s"get_pin ${it.getRtlPath()}$busPostfix"
    } else if(it.isReg && traceOutput) {
      s"get_pin ${it.getRtlPath()}_reg$busPostfix/Q"
    } else if(it.isReg && !traceOutput) {
      s"get_pin ${it.getRtlPath()}_reg$busPostfix/D"
    } else {
      s"get_port ${it.getRtlPath()}$busPostfix"
    }
  }

  def getDrivenRegs(from: BaseType): Seq[BaseType] = {
    val result = mutable.ArrayBuffer[BaseType]()
    from.component.dslBody.foreachStatements {x => x match {
      case das: DataAssignmentStatement if das.source == from =>
        assert(das.finalTarget.isReg, "CDC path not driving a reg")
        result += das.finalTarget
      case _ =>
    }}
    result.toSeq
  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_false_path
  def writeFalsePathFromTo(s: DataAssignmentStatement, writer: Writer): Unit = {
    val source = s.source.asInstanceOf[BaseType]
    val target = s.target.asInstanceOf[BaseType]
    println(source, source.getSingleDriver, !source.isDirectionLess)
    println(target)
    val sourceResolved = if(source.getSingleDriver.isEmpty && source.isInputOrInOut) source else source.getSingleDriver.get
    val targetResolved = getDrivenRegs(target)
    assert(targetResolved.length == 1, "more than one reg driven by false_path path")
    // TODO multiple targets?
    // TODO source driven from reg?
    writer.write(
      s"""
      |# CDC constaints for ${s.source.toString} -> ${s.target.toString} in ${s.component.getPath()}
      |# source: ${Option(s.locationString).getOrElse("unknown, no location string")}
      |set_false_path -from [${portOrPinSelector(sourceResolved)}] -to [${portOrPinSelector(targetResolved.head)}]
      |""".stripMargin
    )
  }

  def writeFalsePathThrough(s: DataAssignmentStatement, writer: Writer): Unit = {
    val source = s.source.asInstanceOf[BaseType]
    val target = s.target.asInstanceOf[BaseType]
    writer.write(
      s"""
         |# CDC constaints for ${s.source.toString} -> ${s.target.toString} in ${s.component.getPath()}
         |# source: ${Option(s.locationString).getOrElse("unknown, no location string")}
         |set_false_path -through ${source.getRtlPath()}
         |""".stripMargin
    )
  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_max_delay
  // and https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_bus_skew
  def writeMaxDelay(s: DataAssignmentStatement, tag: crossClockMaxDelay, writer: Writer): Unit = {
    val source = s.source.asInstanceOf[BaseType]
    val target = s.target.asInstanceOf[BaseType]
    // TODO trace source to previous FF
    // TODO fix constraint to find pin
    val sourceResolved = if(source.getSingleDriver.isEmpty && source.isInputOrInOut) source else source.getSingleDriver.get
    val targetResolved = getDrivenRegs(target)
    assert(targetResolved.length == 1)
    val blub = false
    writer.write(
      s"""
      |# CDC constraints for ${s.source.toString} -> ${s.target.toString} in ${s.component.getPath()}
      |# source: ${Option(s.locationString).getOrElse("unknown, no location string")}""".stripMargin
    )
    if(!blub) {
      writer.write(
        s"""
        |set src_clk [get_clocks -quiet -of [get_ports ${source.component.getRtlPath() + "/" + source.clockDomain.clock.getName()}]]
        |set dst_clk [get_clocks -quiet -of [get_ports ${portOrPinSelector(targetResolved.head, true)}]]
        |set src_clk_period [get_property -quiet -min PERIOD $$src_clk]
        |set dst_clk_period [get_property -quiet -min PERIOD $$dst_clk]
        |if {$$src_clk == ""} {
        |  set src_clk_period 1000
        |}
        |if {$$dst_clk == ""} {
        |  set dst_clk_period 1001
        |}
        |if {($$src_clk != $$dst_clk) || ($$src_clk == "" && $$dst_clk == "")} {
        |""".stripMargin)
    }
    val delay = if(!blub) "$src_clk_period" else (source.clockDomain.frequency.getValue/(1 ns)).formatted("%f")
    val skew = if(!blub) "[expr min ($src_clk_period, $dst_clk_period)]" else (source.clockDomain.frequency.getValue.min(source.clockDomain.frequency.getValue) / (1 ns)).formatted("%f")
    writer.write(
      s"""
      |  set_max_delay -from [${portOrPinSelector(sourceResolved)}] -to [${portOrPinSelector(targetResolved.head)}] $delay -datapath_only
      |  set_bus_skew -from [${portOrPinSelector(sourceResolved)}] -to [${portOrPinSelector(targetResolved.head)}] $skew
      |""".stripMargin
    )
    if(!blub) {
      writer.write(
        s"""}
        |# TODO waive warning
        |
        """.stripMargin
      )
    }
  }

  def fullPath(bt: BaseType) =
    (if (bt.component != null) bt.component.getPath() + "/" else "") + bt.getDisplayName()
}

object BufferCC2 {
  def apply[T <: Data](
      input: T,
      init: => T = null,
      bufferDepth: Int = 2,
      randBoot: Boolean = false,
      inputAttributes: Seq[SpinalTag] = List()
  ): T = {
    val c = new BufferCC2(input, init, bufferDepth, randBoot, inputAttributes)
    c.setCompositeName(input, "buffercc", true)
    c.io.dataIn := input

    val ret = cloneOf(c.io.dataOut)
    ret := c.io.dataOut
    return ret
  }
  val defaultDepth = ScopeProperty(2)
}
class BufferCC2[T <: Data](
    dataType: T,
    init: => T,
    bufferDepth: Int = 2,
    randBoot: Boolean = false,
    inputAttributes: Seq[SpinalTag] = List()
) extends Component {
  assert(bufferDepth >= 1)

  val io = new Bundle {
    val dataIn = in(cloneOf(dataType))
    val dataOut = out(cloneOf(dataType))
  }
  val buffers = Vec(Reg(dataType, init), bufferDepth)
  if (randBoot) buffers.foreach(_.randBoot())

  buffers(0) := io.dataIn
  buffers(0).addTag(crossClockDomain)
  inputAttributes.foreach(io.dataIn.addTag)
  for (i <- 1 until bufferDepth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)
  }
  io.dataOut := buffers.last
}

case class Test() extends Component {

  import spinal.lib._

  val io = new Bundle {
    val i = in port Bool()
    val o = out port Bool()

    val is = slave port Stream(Bits(2 bit))
    val os = master port Stream(Bits(2 bit))

    val ib = in port Bits(3 bit)
    val ob = out port Bits(3 bit)

    val iv = in port Vec(Bits(2 bit), 2)
    val ov = out port Vec(Bits(2 bit), 2)
  }
  val otherCD = ClockDomain.external("someDomain")

  io.o := BufferCC2(io.i, inputAttributes = List(crossClockFalsePath))
  io.os <> io.is.ccToggle(ClockDomain.current, otherCD)

  otherCD {
    io.ob := BufferCC2(
      io.ib,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )

    io.ov := BufferCC2(
      io.iv,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )
  }
}

object Test extends App {
  val report = SpinalVerilog(Test())
  DumpAST(report.toplevel)
  ConstraintWriter(report.toplevel, filename = "test.xdc")
  if(false)
    VivadoFlow(
      "/opt/Xilinx/Vivado/2022.1/bin/",
      "benchmarkWorkspace",
      Rtl(report),
      additionalXdc=List(new File("test.xdc").getAbsolutePath),
      verbose=true
    )
}
