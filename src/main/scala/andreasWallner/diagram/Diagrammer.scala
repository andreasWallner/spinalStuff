package andreasWallner.diagram

import spinal.core.{BaseType, Component}
import spinal.core.internals.{
  AssignmentExpression,
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
  SubAccess,
  SwitchStatement,
  SwitchStatementKeyBool,
  TreeStatement,
  UnaryOperator,
  WhenStatement
}

import java.io.{File, PrintWriter, Writer}

case class Diagrammer(c: Component) {
  def draw(filename: String, openViewer: Boolean = false): Unit = {
    val writer = new PrintWriter(new File(filename))
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
    draw(c, writer)
    writer.write("}")
    writer.close()

    if (openViewer) {
      import sys.process._
      println(s"dot -Tx11 $filename" !!)
    }
  }

  def draw(c: Component, w: Writer): Unit = {
    w.write(
      s"""subgraph cluster_${c.hashCode()} {
         |  label = "${c.getName()}"
         |  style = solid
         |""".stripMargin)

    def connectReg(x: BaseType) = {
      w.write(
        s"""  sig_${x.clockDomain.clock.hashCode()} -> sig_${
          x
            .hashCode()
        } [color=red]\n""")
      if (x.clockDomain.hasClockEnableSignal)
        w.write(
          s"""  sig_${x.clockDomain.clockEnable.hashCode()} -> sig_${
            x
              .hashCode()
          } [color=aqua]\n""")
      if (x.clockDomain.hasResetSignal)
        w.write(
          s"""  sig_${x.clockDomain.reset.hashCode()} -> sig_${
            x
              .hashCode()
          } [color=crimson]\n""")
      if (x.clockDomain.hasSoftResetSignal)
        w.write(
          s"""  sig_${x.clockDomain.softReset.hashCode()} -> sig_${
            x
              .hashCode()
          } [color=brown]\n""")
    }

    for (x <- c.getAllIo) {
      val (rank, color, shape) = x match {
        case xx if xx.isOutput => ("sink", "#e2cbc1", "rectangle")
        case xx if xx.isInOut => ("sink", "#e2cbc1", "circle")
        case xx if xx.isAnalog => ("sink", "#e2cbc1", "doublecircle")
        case xx if xx.isInput => ("source", "#b5fed9", "rectangle")
      }
      val style = if (x.isReg) "filled, bold" else "filled"
      w.write(
        s"""  { rank=$rank; sig_${x.hashCode()} [label="${
          x
            .getName()
        }" style="$style" fillcolor="$color" shape=$shape] }\n""")
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
          println(s"o  ${io.getClass} $io ${io.hashCode()}")
          println()
        case io: BaseType if !io.isDirectionLess =>
          println(s"io ${io.getClass} $io ${io.hashCode()}")
        case l: Literal =>
          w.write(s""" {sig_${l.hashCode()} [label="${l.getValue()}"]}\n""")
        case uop: UnaryOperator =>
          w.write(
            s"""  {sig_${
              uop
                .hashCode()
            } [label="${uop.opName}" style="$style"]}\n""")
          w.write(
            s"""  sig_${uop.source.hashCode()} -> sig_${uop.hashCode()}\n"""
          )
        case bop: BinaryOperator =>
          w.write(
            s"""  {sig_${
              bop
                .hashCode()
            } [label="${bop.opName}" style="$style"]}\n""")
          w.write(
            s"""  sig_${bop.left.hashCode()} -> sig_${bop.hashCode()}\n"""
          )
          w.write(
            s"""  sig_${bop.right.hashCode()} -> sig_${bop.hashCode()}\n"""
          )
        case cop: ConstantOperator => ???

        case c: Cast =>
          w.write(
            s"""  {sig_${
              c
                .hashCode()
            } [label="${c.opName}" style="$style"]}\n""")
          w.write(s"""  sig_${c.input.hashCode()} -> sig_${c.hashCode()}\n""")

        case a: BitVectorBitAccessFixed => // stop decent here?
          w.write(
            s"""  {sig_${
              a
                .hashCode()
            } [label="x[${a.bitId}]" style="$style"]}\n""")
          w.write(s"""  sig_${a.source.hashCode()} -> sig_${a.hashCode()}\n""")
        case a: BitVectorRangedAccessFixed =>
          w.write(
            s"""  {sig_${
              a
                .hashCode()
            } [label="x[${a.hi}:${a.lo}]" style="$style"]}\n""")
          w.write(s"""  sig_${a.source.hashCode()} -> sig_${a.hashCode()}\n""")

        case bt: BaseType =>
          println(s"! ${bt.getClass} $bt ${bt.hashCode()}")
        case o => println(s"${o.getClass} $o")
      }
      // go deeper
    }

    c.dslBody.walkStatements {
      case d: BaseType with DeclarationStatement =>
        val style = if (d.isReg) "bold" else ""
        w.write(
          s"""  {sig_${d.hashCode()} [label="${
            d
              .getName()
          }" style="$style"]}\n""")
      case da: DataAssignmentStatement =>
        da.walkDrivingExpressions(writeTargets)
        w.write(
          s"""  sig_${da.source.hashCode()} -> sig_${da.target.hashCode()}"""
        )

      case ia: InitAssignmentStatement =>
      case s: SwitchStatement =>
        w.write(s"""  {sig_${s.hashCode()} [label="switch" color=blue]}\n""")
      case w: WhenStatement =>
        println(s">> w ${w}")
      case t =>
        println(s">> t ${t}")
    }

    if (c.children.nonEmpty) {
      w.write("{\n")
      c.children.foreach(cc => draw(cc, w))
      w.write("}\n")
    }

    w.write("}\n")
  }
}
