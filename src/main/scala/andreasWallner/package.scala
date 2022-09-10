package andreasWallner

import scala.collection.mutable

object Utils {
  def lcm(a: BigInt, b: BigInt): BigInt = {
    if(a.signum == 0 || b.signum == 0)
      return BigInt(0)
    a / a.gcd(b) * b
  }
  def lcm(a: Int, b: Int): Int = lcm(BigInt(a), BigInt(b)).intValue()
  def lcm(a: Long, b: Long): Long = lcm(BigInt(a), BigInt(b)).intValue()
  def lcm(xs: BigInt*): BigInt = { // TODO check if first elem is negative
    if (xs.isEmpty)
      return BigInt(1)
    var lcm = xs(0)
    for (v <- xs.drop(1)) {
      if (v.signum != 0) {
        val gcd = lcm.gcd(v)
        if (gcd == 1) {
          lcm = lcm * v
        } else if (v != gcd) {
          lcm = lcm * (v/gcd)
        }
      }
    }
    lcm
  }

  def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)
  def gcd(a: Int, b: Int): Int = gcd(BigInt(a), BigInt(b)).intValue()
  def gcd(a: Long, b: Long): Long = gcd(BigInt(a), BigInt(b)).longValue()

  // Use for memoization, e.g. during simulation
  // Example:
  //   lazy val dutFactory: ((Int, Int)) => SimCompiled[MemoryFormatterTester] =
  //     memoize { case (i, o) =>
  //       SimConfig.withFstWave.compile(
  //         Comp(i, o).setDefinitionName(f"Comp_${i}_$o"))}
  //
  // Note: current implementation is not thread safe
  //
  // Implementation taken from pathikrit (https://stackoverflow.com/a/36960228, CC-BY-SA 3.0)
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}
