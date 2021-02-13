package andreasWallner.xilinx

import org.scalatest._
import flatspec._
import matchers._

class TestHelper extends AnyFlatSpec with should.Matchers {
  "engineeringNotation" should "format correctly" in {
    import Helper._
    engineeringNotation(BigDecimal("1000"), "Hz") should be("1k")
    engineeringNotation(BigDecimal("1123"), "Hz") should be("1k123")
    engineeringNotation(BigDecimal("1500"), "Hz") should be("1k5")
    engineeringNotation(BigDecimal("3.3"), "V") should be("3V3")
    engineeringNotation(BigDecimal("0.00001"), "Hz") should be("10u")
    engineeringNotation(BigDecimal("32768"), "Hz") should be("32k768")
    engineeringNotation(BigDecimal("32768"), "Hz", 1) should be("32k8")
  }
}
