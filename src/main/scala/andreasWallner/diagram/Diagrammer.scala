package andreasWallner.diagram

import spinal.core.{BaseType, Component, Data, ExternalDriverTag}
import spinal.core.internals.{
  AssignmentExpression,
  BinaryMultiplexer,
  BinaryOperator,
  BitVectorBitAccessFixed,
  BitVectorRangedAccessFixed,
  BitsBitAccessFixed,
  Cast,
  CastUIntToBits,
  ConstantOperator,
  DataAssignmentStatement,
  DeclarationStatement,
  Expression,
  InitAssignmentStatement,
  LeafStatement,
  Literal,
  Operator,
  Resize,
  ScopeStatement,
  Statement,
  SubAccess,
  SwitchStatement,
  SwitchStatementKeyBool,
  TreeStatement,
  UnaryOperator,
  WhenStatement
}

import java.io.{File, PrintWriter, Writer}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
    writer.write(s"""{ subgraph cluster_$id {
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

  def mux2(x: Expression): Unit = {
    val id = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    writer.write(s"""{ $id [shape=record, label="{{<t> T|<sel> ?|<f> F}|<out> =}"] }\n""")
  }

  def mux3col(x: Expression, i: Iterable[Iterable[String]]) = {
    val values = i.zipWithIndex
      .map { case (ii, idx) => s"""{<e$idx> =|{${ii.mkString("|")}}}""" }
      .mkString("|")
    val id = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    writer.write(s"""{ $id [shape=record, label="{{<sel> ?|$values}|=}"] }\n""")
  }

  def ff(
      x: Expression,
      hasRst: Boolean = false,
      hasEn: Boolean = false,
      hasSoftRst: Boolean = false
  ) = {
    val id = s"sig_${Integer.toHexString(System.identityHashCode(x))}"
    val inputs = "{<d> D|" +
      (if (hasRst) "<rst> RST|" else "") +
      (if (hasEn) "<en> EN|" else "") +
      (if (hasSoftRst) "<srst> SRST|" else "") +
      "<clk> CLK}"
    writer.write(s"""{ $id [shape=record, label={$inputs|FF}""")
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
      graph.node(x, rank, "style" -> style, "fillcolor" -> color, "shape" -> shape)
      if (x.isReg)
        connectReg(x)
    }

    def writeStatic(e: Expression): Unit = {
      val style = e match {
        case bt: BaseType if bt.isReg =>
          connectReg(bt)
          "bold"
        case _ => ""
      }
      e match {
        case l: Literal =>
          graph.node(l, "label" -> l.getValue().toString()) // TODO intelligent radix?
        case uop: UnaryOperator =>
          graph.node(uop, "label" -> uop.opName, "style" -> style)
          graph.connection(uop.source, uop)
        case bop: BinaryOperator =>
          graph.node(bop, "label" -> bop.opName, "style" -> style)
          graph.connection(bop.left, bop)
          graph.connection(bop.right, bop)
        case cop: ConstantOperator => ???

        case c: Cast =>
          graph.node(c, "label" -> c.opName, "style" -> style)
          graph.connection(c.input, c)

        case a: BitVectorBitAccessFixed => // stop decent here?
          graph.node(a, "label" -> s"x[${a.bitId}", "style" -> style)
          graph.connection(a.source, a)
        case a: BitVectorRangedAccessFixed =>
          graph.node(a, "label" -> s"x[${a.hi}:${a.lo}]", "style" -> style)
          graph.connection(a.source, a)

        case bm: BinaryMultiplexer =>
          graph.mux2(bm)
          graph.connection(bm.whenTrue, bm, "t")
          graph.connection(bm.whenFalse, bm, "f")
          graph.connection(bm.cond, bm, "sel")

        case bt: BaseType =>
        //println(s"not implemented ! ${bt.getClass} $bt ${bt.hashCode()}")
        case o => //println(s"unknown ! ${o.getClass} $o ${o.hashCode()}")
      }
      // go deeper
    }

    implicit class ScopeStatementPimper(ss: ScopeStatement) {
      def mapStatements[B](f: Statement => B): ArrayBuffer[B] = {
        val b = ArrayBuffer[B]()
        ss.foreachStatements(s => b += f(s))
        b
      }
      def exists(f: Statement => Boolean): Boolean = {
        var ptr = ss.head
        while (ptr != null) {
          if (f(ptr))
            return true
          ptr = ptr.nextScopeStatement
        }
        false
      }
      def filter(f: Statement => Boolean): Seq[Statement] = {
        val b = ArrayBuffer[Statement]()
        ss.foreachStatements(s => if (f(s)) b += s)
        b.toSeq
      }
    }

    def writeSimpleSwitch(s: SwitchStatement, target: Expression): Unit = {
      graph.mux3col(
        target,
        s.elements.map(e => e.keys.map(k => k.asInstanceOf[Literal].getValue().toString()))
      ) // TODO nicer printing...
      graph.connection(s.value, target, "sel")
      s.elements.zipWithIndex.foreach {
        case (e, idx) =>
          val das = e.scopeStatement
            .filter(s => s.asInstanceOf[DataAssignmentStatement].target == target)
            .head
            .asInstanceOf[DataAssignmentStatement]
          graph.connection(das.source, target, s"e$idx")
      }
    }

    val assignments = mutable.HashMap[Expression, ArrayBuffer[DataAssignmentStatement]]()
    def collectAndWriteStatic(s: Statement): Unit = {
      s match {
        case d: BaseType with DeclarationStatement =>
          val style = if (d.isReg) "bold" else ""
          graph.node(d, "label" -> d.getName(), "style" -> style)
          if (d.hasTag(classOf[ExternalDriverTag]))
            graph.connection(d.getTag(classOf[ExternalDriverTag]).get.driver, d)
        case da: DataAssignmentStatement =>
          da.walkDrivingExpressions(writeStatic)
          assignments.getOrElseUpdate(da.target, ArrayBuffer()).append(da)
        case ss: SwitchStatement =>
        //ss.walkStatements(collectAndWriteStatic)
        case _ =>
      }
    }
    c.dslBody.walkStatements(collectAndWriteStatic)

    def getSingleSwitch(das: DataAssignmentStatement): Option[SwitchStatement] = {
      var firstSwitch: SwitchStatement = null
      das.walkParentTreeStatements {
        case ss: SwitchStatement if firstSwitch == null => firstSwitch = ss
        case _: SwitchStatement                         => return None
      }
      Some(firstSwitch)
    }

    for ((target, dass) <- assignments) {
      if (dass.size == 1) {
        graph.connection(dass(0).source, dass(0).target)
        // next: distinguish between register and register w/ enable
      } else {
        // simple switch?
        val scopes = dass.map(getSingleSwitch)
        val isSimpleSwitch = scopes.forall(ss => ss.isDefined && ss.get == scopes.head.get) && scopes.head.get.elements
          .forall(e => e.keys.forall(k => k.isInstanceOf[Literal]))
        if (isSimpleSwitch)
          writeSimpleSwitch(scopes.head.get, target)

        // TODO continue my dumb "synthesis"
      }
    }

    if (c.children.nonEmpty)
      c.children.foreach(cc => draw(cc, graph))

    graph.closeSubgraph()
  }
}
