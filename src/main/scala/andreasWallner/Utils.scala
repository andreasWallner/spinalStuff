package andreasWallner

import scala.collection.mutable

object Utils {

  /** calculate least common multiple (the smallest number that both a & b divide w/o remainder) */
  def lcm(a: BigInt, b: BigInt): BigInt = {
    if (a.signum == 0 || b.signum == 0)
      return BigInt(0)
    a / a.gcd(b) * b
  }
  def lcm(a: Int, b: Int): Int = lcm(BigInt(a), BigInt(b)).intValue()
  def lcm(a: Long, b: Long): Long = lcm(BigInt(a), BigInt(b)).longValue()
  def lcm(xs: BigInt*): BigInt = {
    if (xs.isEmpty)
      return BigInt(1)
    assert(xs.head >= 0, "First passed BigInt must be positive")
    var lcm = xs(0)
    for (v <- xs.drop(1)) {
      if (v.signum != 0) {
        val gcd = lcm.gcd(v)
        if (gcd == 1) {
          lcm = lcm * v
        } else if (v != gcd) {
          lcm = lcm * (v / gcd)
        }
      }
    }
    lcm
  }

  /** calculate greatest common divisor (the biggest number that divides both a & b w/o remainder) */
  def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)
  def gcd(a: Int, b: Int): Int = gcd(BigInt(a), BigInt(b)).intValue()
  def gcd(a: Long, b: Long): Long = gcd(BigInt(a), BigInt(b)).longValue()

  /**
    * Use for memoization, e.g. during simulation
    *
    * Example:
    *   lazy val dutFactory: ((Int, Int)) => SimCompiled[MemoryFormatterTester] =
    *     memoize { case (i, o) =>
    *       SimConfig.withFstWave.compile(
    *         Comp(i, o).setDefinitionName(f"Comp_${i}_$o"))}
    *
    *  Note: current implementation is not thread safe
    *  Implementation taken from pathikrit (https://stackoverflow.com/a/36960228, CC-BY-SA 3.0)
    */
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  class DumpAST(indent: String) {
    import spinal.core._
    import spinal.core.internals._
    private lazy val indented = new DumpAST(indent + "  ")

    def printFlags(d: Data): Unit = {
      print(if (d.isInOut) d.dirString() + " " else "")
      print(if (d.isAnalog) "ANA " else "")
      print(if (d.isReg) s"REG " else "")
    }
    def printAttributes(bt: BaseType): Unit = {
      printFlags(bt.asInstanceOf[Data])
      print(if (bt.isReg) s"${bt.clockDomain} " else "")
    }

    val tMark = s"${Console.GREEN}T${Console.RESET}"
    def printTags(str: SpinalTagReady): Unit = {
      for (tag <- str.getTags()) {
        println(s"$tMark $indent ${tag.getClass.getName} ${tag.toString}")
      }
    }
    def printTags(bn: BaseNode): Unit = {
      bn match {
        case str: SpinalTagReady => printTags(str.asInstanceOf[SpinalTagReady])
        case _ =>
      }
    }

    val eMark = s"${Console.YELLOW}E${Console.RESET}"
    def dump(e: Expression): Unit = {
      print(s"$eMark $indent ${e.getClass.toString} ${e.toString} (")
      e match {
        case bt: BaseType => printAttributes(bt)
        case d: Data      => printFlags(d)
        case _            =>
      }
      println(")")
      indented.printTags(e)
      e.foreachExpression(indented.dump)
    }

    val sMark = s"${Console.RED}S${Console.RESET}"
    def dump(s: Statement): Unit = {
      print(s"$sMark $indent ${s.getClass.toString} ${s.toString} (")
      s match {
        case bt: BaseType => printAttributes(bt)
        case d: Data      => printFlags(d)
        case _            =>
      }
      println(")")
      s match {
        case das: DataAssignmentStatement =>
          new DumpAST(indent + "   source ").dump(das.source)
          new DumpAST(indent + "   target ").dump(das.target)
        case _ => s.foreachExpression(indented.dump)
      }
      indented.printTags(s)
    }

    val cMark = s"${Console.BLUE}C${Console.RESET}"
    def dump(c: Component): Unit = {
      println(s"$cMark $indent ${c.toString()}")
      indented.printTags(c)
      c.dslBody.foreachStatements(indented.dump)
      c.children.foreach(indented.dump)
    }
  }
  object DumpAST {
    import spinal.core.Component
    import spinal.core.internals.{Statement, Expression}
    private val d = new DumpAST("")
    def apply(c: Component): Unit = d.dump(c)
    def apply(s: Statement): Unit = d.dump(s)
    def apply(e: Expression): Unit = d.dump(e)
  }
}
