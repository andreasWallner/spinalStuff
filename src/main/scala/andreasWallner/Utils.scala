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

  def divCeil(p: Int, q: Int): Int = {
    assert(p >= 0)
    assert(q > 0)
    (p + (q - 1)) / q
  }

  object isPow2 {
    def apply(bi: BigInt): Boolean = bi > 0 && ((bi & (bi - 1)) == 0)
    def apply(i: Int): Boolean = isPow2(BigInt(i))
  }

  object popCount {
    def apply(b: Byte): Integer = {
      val x0 = ((b & 0xaa) >> 1) + (b & 0x55)
      val x1 = ((x0 & 0xcc) >> 2) + (x0 & 0x33)
      ((x1 & 0xf0) >> 4) + (x1 & 0x0f)
    }
    def apply(bi: BigInt): Integer = bi.toByteArray.foldLeft(0)(_ + popCount(_))
  }

  object oddParity {

    /**
      * Calculate the parity bit for odd parity
      *
      * Warning: does not check the parity, but calculate it
      */
    def apply(b: Byte): Boolean = (0 until 8).map(s => (b >> s) & 1).reduce(_ ^ _) == 0
    def apply(i: Int): Boolean = (0 until 32).map(s => (i >> s) & 1).reduce(_ ^ _) == 0
    def apply(bi: BigInt): Boolean = (popCount(bi) & 1) == 0
  }

  object evenParity {

    /**
      * Calculate the parity bit for even parity
      *
      * Warning: does not check the parity, but calculate it
      */
    def apply(b: Byte): Boolean = !oddParity(b)
    def apply(i: Int): Boolean = !oddParity(i)
    def apply(bi: BigInt): Boolean = !oddParity(bi)
  }

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
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  class DumpAST(indent: String, withHashCode: Boolean = false) {
    import spinal.core._
    import spinal.core.internals._
    private lazy val cleanIndent = " " * (indent.length - 1 + 2)
    private lazy val indented = new DumpAST(indent + "  ", withHashCode)

    def hash(x: Any): String = {
      if (withHashCode) {
        s"${Integer.toHexString(System.identityHashCode(x))} "
      } else {
        ""
      }
    }
    def printFlags(d: Data): Unit = {
      print(if (d.isInOut) d.dirString() + " " else "")
      print(if (d.isAnalog) "ANA " else "")
      print(if (d.isReg) s"REG " else "")
    }
    def printAttributes(bt: BaseType): Unit = {
      printFlags(bt.asInstanceOf[Data])
      print(if (bt.isReg) s"${bt.clockDomain} ${Integer.toHexString(bt.clockDomain.clock.hashCode())}" else "")
    }

    val tMark = s"${Console.GREEN}T${Console.RESET}"
    def printTags(str: SpinalTagReady): Unit = {
      for (tag <- str.getTags()) {
        println(s"$tMark $indent ${tag.getClass.getName} ${tag.toString}")
        tag match {
          case edt: ExternalDriverTag => println(indent + " driver" + edt.driver.toString + " " + Integer.toHexString(edt.driver.hashCode()))
          case _ =>
        }

      }
    }
    def printTags(bn: BaseNode): Unit = {
      bn match {
        case str: SpinalTagReady => printTags(str.asInstanceOf[SpinalTagReady])
        case _                   =>
      }
    }

    val eMark = s"${Console.YELLOW}E${Console.RESET}"
    def dump(e: Expression): Unit = {
      print(s"$eMark $indent ${e.getClass.toString} ${e.toString} ${hash(e)}(")
      e match {
        case bt: BaseType => printAttributes(bt)
        case d: Data      => printFlags(d)
        case _            =>
      }
      println(")")
      indented.printTags(e)
      e match {
        case bm: BinaryMultiplexer =>
          new DumpAST(cleanIndent + "?").dump(bm.cond)
          new DumpAST(cleanIndent + "T").dump(bm.whenTrue)
          new DumpAST(cleanIndent + "F").dump(bm.whenFalse)
        case _ => e.foreachExpression(indented.dump)
      }
    }

    val sMark = s"${Console.RED}S${Console.RESET}"
    def dump(s: Statement): Unit = {
      print(s"$sMark $indent ${s.getClass.toString} ${s.toString} ${hash(s)}(")
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
        case ss: SwitchStatement =>
          new DumpAST(cleanIndent + "  value ").dump(ss.value)
          ss.elements.foreach { se =>
            se.keys.foreach(new DumpAST(cleanIndent + "  is ").dump)
            se.scopeStatement.foreachStatements(new DumpAST(cleanIndent + "    then").dump)
          }
        case _ => s.foreachExpression(indented.dump)
      }
      indented.printTags(s)
      s match {
        case s: TreeStatement => s.foreachStatements(indented.dump)
        case _                =>
      }
    }

    val cMark = s"${Console.BLUE}C${Console.RESET}"
    def dump(c: Component): Unit = {
      println(s"$cMark $indent ${c.getName()} ${hash(c)} ")
      indented.printTags(c)
      c.dslBody.foreachStatements(indented.dump)
      c.children.foreach(indented.dump)
    }
  }
  object DumpAST {
    import spinal.core.Component
    import spinal.core.internals.{Statement, Expression}
    def apply(c: Component): Unit = apply(c, withHashCode = false)
    def apply(c: Component, withHashCode: Boolean): Unit =
      new DumpAST("", withHashCode).dump(c)
    def apply(s: Statement): Unit = apply(s, withHashCode = false)
    def apply(s: Statement, withHashCode: Boolean): Unit =
      new DumpAST("", withHashCode).dump(s)
    def apply(e: Expression): Unit = apply(e, withHashCode = false)
    def apply(e: Expression, withHashCode: Boolean): Unit =
      new DumpAST("", withHashCode).dump(e)
  }
}
