package andreasWallner

import spinal.core.PhysicalNumber

package object ext {
  implicit class PimpedPhysicalNumber[T <: PhysicalNumber[_]](num: PhysicalNumber[T]) {
    def <(that: T): Boolean = num.toBigDecimal < that.toBigDecimal

    def <=(that: T): Boolean = num.toBigDecimal <= that.toBigDecimal

    def >(that: T): Boolean = num.toBigDecimal > that.toBigDecimal

    def >=(that: T): Boolean = num.toBigDecimal >= that.toBigDecimal

    def ==(that: T): Boolean = num.toBigDecimal == that.toBigDecimal

    def <(that: BigDecimal) = num.toBigDecimal < that

    def <=(that: BigDecimal) = num.toBigDecimal <= that

    def >(that: BigDecimal) = num.toBigDecimal > that

    def >=(that: BigDecimal) = num.toBigDecimal >= that

    def ==(that: BigDecimal) = num.toBigDecimal == that

    def min(that: T): T = num.newInstance(that.toBigDecimal.min(num.toBigDecimal))

    def abs: T = num.newInstance(num.toBigDecimal.abs)
  }

}
