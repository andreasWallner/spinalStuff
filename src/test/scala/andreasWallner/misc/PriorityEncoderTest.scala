package andreasWallner.misc

import andreasWallner.SpinalFunSuite
import andreasWallner.sim._
import spinal.core.sim._

import scala.language.postfixOps
import org.scalactic.TimesOnInt.convertIntToRepeater
import org.scalatest.Ignore
import spinal.lib.BigIntRicher

class PriorityEncoderTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(PriorityEncoder(5))

  test(dut, "simple") { dut =>
    1000 times {
      val value = dut.io.bits.randomize()
      sleep(1)

      if(value != 0)
        assert(dut.io.encoded.toInt == value.lowestSetBit)
      assert(dut.io.valid.toBoolean == (value != 0))
    }
  }
}

@Ignore("does not work yet")
class RecursivePriorityEncoderTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(RecursivePriorityEncoder(16, 4))

  test(dut, "simple") { dut =>
    1000 times {
      val value = dut.io.bits.randomize()
      sleep(1)

      simLog(value.binString(), value.lowestSetBit, dut.io.valid.toBoolean, dut.io.encoded.toInt)
      if(value != 0)
        assert(dut.io.encoded.toInt == value.lowestSetBit)
      assert(dut.io.valid.toBoolean == (value != 0))
    }
  }
}
