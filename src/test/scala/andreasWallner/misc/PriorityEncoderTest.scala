package andreasWallner.misc

import spinal.core._
import spinal.core.sim._
import spinal.core.formal._
import spinal.lib._

import andreasWallner.{SpinalFormalFunSuite, SpinalFunSuite}
import andreasWallner.sim._

import scala.language.postfixOps
import org.scalactic.TimesOnInt.convertIntToRepeater
import org.scalatest.Ignore

class PriorityEncoderTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(PriorityEncoder(5))

  test(dut, "simple") { dut =>
    1000 times {
      val value = dut.io.bits.randomize()
      sleep(1)

      if (value != 0)
        assert(dut.io.encoded.toInt == value.lowestSetBit)
      assert(dut.io.valid.toBoolean == (value != 0))
    }
  }
}

@Ignore
class RecursivePriorityEncoderTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(RecursivePriorityEncoder(16, 4))

  test(dut, "simple") { dut =>
    1000 times {
      val value = dut.io.bits.randomize()
      sleep(1)

      simLog(value.binString(), value.lowestSetBit, dut.io.valid.toBoolean, dut.io.encoded.toInt)
      if (value != 0)
        assert(dut.io.encoded.toInt == value.lowestSetBit)
      assert(dut.io.valid.toBoolean == (value != 0))
    }
  }
}

class RecursivePriorityGateTest extends SpinalFormalFunSuite {
  def countBits(bv: BitVector): UInt = {
    (0 until bv.getWidth).foldLeft(U(0, log2Up(bv.getWidth) bit))((u, idx) =>
      u + bv(idx).asUInt(bv.getWidth bit)
    )
  }
  case class Test() extends Component {
    val prioWidth = 3
    val width = 5

    val dut = FormalDut(RecursivePriorityGate(prioWidth, width))
    assumeInitial(ClockDomain.current.readResetWire)
    anyseq(dut.io.signals)
    anyseq(dut.io.priorities)

    val combined = Vec
      .tabulate(width) { i =>
        (dut.io.signals(i) ## dut.io.priorities(i) ## U(i, prioWidth bit)).asUInt
      }
    val best = combined.reduceBalancedTree((a, b) => (a > b) ? a | b)
    val chosenIndex = best.takeLow(prioWidth).asUInt

    // either no signals gets through or the one chosen bit needs to be set
    assert(best.msb === False || dut.io.masked(chosenIndex) === True)
    // onehot if any is set, all 0 otherwise
    assert(countBits(dut.io.masked) === best.msb.asUInt)

    // low prio wins
    cover(dut.io.priorities(0) === 0 && dut.io.priorities(4) === 5 && dut.io.masked(0) === True)
    // high prio wins
    cover(dut.io.priorities(0) === 0 && dut.io.priorities(4) === 5 && dut.io.masked(4) === True)
    // prio wins over all bits set
    cover(
      dut.io.priorities(0) === 4 && dut.io.priorities(1) === 4 &&
        dut.io.priorities(2) === 4 && dut.io.priorities(4) === 4 &&
        dut.io.masked(3) === True && dut.io.signals === B"11111"
    )
  }

  test("all") {
    SpinalFormalConfig(_keepDebugInfo = true).withBMC(5).withProve(5).withCover(5).doVerify(Test())
  }
}
