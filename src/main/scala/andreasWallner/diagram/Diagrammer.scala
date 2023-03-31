package andreasWallner.diagram

import spinal.core.{BaseType, Component, Data, ExternalDriverTag}
import spinal.core.internals.{AssignmentExpression, BinaryMultiplexer, BinaryOperator, BitVectorBitAccessFixed, BitVectorRangedAccessFixed, BitsBitAccessFixed, Cast, CastUIntToBits, ConstantOperator, DataAssignmentStatement, DeclarationStatement, Expression, InitAssignmentStatement, LeafStatement, Literal, Operator, Resize, SubAccess, SwitchStatement, SwitchStatementKeyBool, TreeStatement, UnaryOperator, WhenStatement}

import java.io.{File, PrintWriter, Writer}
import scala.language.postfixOps

class Graph(writer: Writer) {
  def openDigraph(): Unit = {
    writer.write(
      """digraph root {
        |  node [fontsize=8 margin=".1,.01" width=.5 height=.5 shape=box]
        |  edge [fontsize=8]
        |  rankdir=LR;
        |  ranksep=.25;
        |  nodesep=.5;
        |
        |""".stripMargin
    )
  }

  def closeDigraph(): Unit = {
    writer.write("}")
  }

  def openSubgraph(id: String, name: String): Unit = {
    writer.write(
      s"""{ subgraph cluster_$id {
         |  label = "$name"
         |  style = solid
         |""".stripMargin)
  }

  def closeSubgraph(): Unit = {
    writer.write("}}")
  }

  def connection(from: Any, to: Any, attributes: (String, String)*): Unit = {
    val f = s"sig_${Integer.toHexString(System.identityHashCode(from))}"
    val t = s"sig_${Integer.toHexString(System.identityHashCode(to))}"
    val attr = attributes.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")
    writer.write(s"""   $f -> $t [$attr]\n""")
  }

  def connection(from: Any, to: Any, toRec: String, attributes: (String, String)*): Unit = {
    val f = s"sig_${Integer.toHexString(System.identityHashCode(from))}"
    val t = s"sig_${Integer.toHexString(System.identityHashCode(to))}:$toRec"
    val attr = attributes.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")
    writer.write(s"""   $f -> $t [$attr]\n""")
  }

  def node(x: Expression, rank: String, attributes: (String, String)*): Unit = { // make rank enum
    val xx = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    val attr = attributes.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")
    writer.write(s"  { rank=$rank; $xx [$attr] }\n")
  }
  def node(x: Expression, attributes: (String, String)*): Unit = {
    val xx = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    val attr = attributes.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")
    writer.write(s"  { $xx [$attr] }\n")
  }

  def node(x: Any, attributes: (String, String)*): Unit = {
    val xx = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    val attr = attributes.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")
    writer.write(s"  { $xx [$attr] }\n")
  }

  def mux2(x: Expression, attributes: (String, String)*): String = {
    val id = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    writer.write(
      s"""{ $id [shape=record, label="{{<t> T|<sel> ?|<f> F}|<out> =}"] }\n""")
    id
  }
}

case class Diagrammer(c: Component) {
  def draw(filename: String, openViewer: Boolean = false): Unit = {
    val writer = new PrintWriter(new File(filename))
    val graph = new Graph(writer)
    graph.openDigraph()
    draw(c, graph)
    graph.closeDigraph()
    writer.close()

    if (openViewer) {
      import sys.process._
      println(s"dot -Tx11 $filename" !!)
    }
  }

  def draw(c: Component, graph: Graph): Unit = {
    graph.openSubgraph(c.hashCode().toString, c.getName())

    def connectReg(x: BaseType): Unit = {
      graph.connection(x.clockDomain.clock, x, "color" -> "red")
      if (x.clockDomain.hasClockEnableSignal)
        graph.connection(x.clockDomain.clockEnable, x, "color" -> "aqua")
      if (x.clockDomain.hasResetSignal)
        graph.connection(x.clockDomain.reset, x, "color" -> "crimson")
      if (x.clockDomain.hasSoftResetSignal)
        graph.connection(x.clockDomain.softReset, x, "color" -> "brown")
    }

    for (x <- c.getAllIo) {
      val (rank, color, shape) = x match {
        case xx if xx.isOutput => ("sink", "#e2cbc1", "rectangle")
        case xx if xx.isInOut  => ("sink", "#e2cbc1", "circle")
        case xx if xx.isAnalog => ("sink", "#e2cbc1", "doublecircle")
        case xx if xx.isInput  => ("source", "#b5fed9", "rectangle")
      }
      val style = if (x.isReg) "filled, bold" else "filled"
      graph.node(x, rank, "style"->style, "fillcolor"->color, "shape"->shape)
      if (x.isReg)
        connectReg(x)
    }

    def writeTargets(e: Expression): Unit = {
      val style = e match {
        case bt: BaseType if bt.isReg =>
          connectReg(bt)
          "bold"
        case _ => ""
      }
      e match {
        case io: BaseType if io.isOutput || io.isInOut || io.isAnalog =>
          //println(s"o  ${io.getClass} $io ${io.hashCode()}")
          //println()
        case io: BaseType if !io.isDirectionLess =>
          //println(s"io ${io.getClass} $io ${io.hashCode()}")
        case l: Literal =>
          graph.node(l, "label"->l.getValue().toString()) // TODO intelligent radix?
        case uop: UnaryOperator =>
          graph.node(uop, "label"->uop.opName, "style"->style)
          graph.connection(uop.source, uop)
        case bop: BinaryOperator =>
          graph.node(bop, "label"->bop.opName, "style"->style)
          graph.connection(bop.left, bop)
          graph.connection(bop.right, bop)
        case cop: ConstantOperator => ???

        case c: Cast =>
          graph.node(c, "label"->c.opName, "style"->style)
          graph.connection(c.input, c)

        case a: BitVectorBitAccessFixed => // stop decent here?
          graph.node(a, "label"->s"x[${a.bitId}", "style"->style)
          graph.connection(a.source, a)
        case a: BitVectorRangedAccessFixed =>
          graph.node(a, "label"->s"x[${a.hi}:${a.lo}]", "style"->style)
          graph.connection(a.source, a)

        case bm: BinaryMultiplexer =>
          graph.mux2(bm)
          graph.connection(bm.whenTrue, bm, "t")
          graph.connection(bm.whenFalse, bm, "f")
          graph.connection(bm.cond, bm, "sel")

        case bt: BaseType =>
          println(s"not implemented ! ${bt.getClass} $bt ${bt.hashCode()}")
        case o => println(s"unknown ! ${o.getClass} $o ${o.hashCode()}")
      }
      // go deeper
    }

    c.dslBody.walkStatements {
      case d: BaseType with DeclarationStatement =>
        val style = if (d.isReg) "bold" else ""
        graph.node(d, "label"->d.getName(), "style"->style)
        if(d.hasTag(classOf[ExternalDriverTag]))
          graph.connection(d.getTag(classOf[ExternalDriverTag]).get.driver, d)
      case da: DataAssignmentStatement =>
        da.walkDrivingExpressions(writeTargets)
        graph.connection(da.source, da.target)

      case ia: InitAssignmentStatement =>
      case s: SwitchStatement =>
        graph.node(s, "label"->"switch", "color"->"blue")
      case w: WhenStatement =>
        println(s">> w ${w}")
      case t =>
        println(s">> t ${t}")
    }

    if (c.children.nonEmpty)
      c.children.foreach(cc => draw(cc, graph))

    graph.closeSubgraph()
  }
}
