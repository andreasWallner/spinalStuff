package andreasWallner.misc

import spinal.core._
import spinal.core.sim._
import spinal.core.formal._
import spinal.lib._
import andreasWallner.{SpinalFormalFunSuite, SpinalFunSuite}
import andreasWallner.sim._

import scala.language.postfixOps
import org.scalactic.TimesOnInt.convertIntToRepeater

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

class PriorityEncoderFormal extends SpinalFormalFunSuite {
  val config = SpinalFormalConfig()
    .withBMC(5)
    .withProve(5)
    .withCover(5)
  test("equivalence") {
    config.doVerify(new Component {
      val dut1 = FormalDut(PriorityEncoder(16))
      val dut2 = FormalDut(RecursivePriorityEncoder(16, 4))

      assumeInitial(ClockDomain.current.readResetWire)

      val input = Bits(16 bit)
      anyseq(input)
      dut1.io.bits := input
      dut2.io.bits := input

      assert(!dut1.io.valid || (dut1.io.encoded === dut2.io.encoded))
      assert(dut1.io.valid === dut2.io.valid)

      cover(dut1.io.encoded === 0)
      cover(dut1.io.encoded === 15)
      cover(dut1.io.encoded === 7)
    })
  }
}

class PriorityGateFormal extends SpinalFormalFunSuite {
  def popcnt(bv: BitVector): UInt = {
    val result = (0 until bv.getWidth).foldLeft(U(0, log2Up(bv.getWidth) bit))((u, idx) =>
      u + bv(idx).asUInt(bv.getWidth bit)
    )
    if (result.isNamed)
      result.setWeakName(bv.getName() + "_popcnt")
    result
  }

  val prioWidth = 3
  val width = 5

  val config = SpinalFormalConfig(_keepDebugInfo = true)
    .withBMC(5)
    .withProve(5)
    .withCover(5)
  test("RecursivePriorityGate") {
    config.doVerify(new Component {
      val dut = FormalDut(RecursivePriorityGate(prioWidth, width))
      formalSetup(
        dut.io.signals,
        dut.io.priorities,
        dut.io.result.signals,
        Some(dut.io.result.any),
        Some(dut.io.result.prio)
      )
    })
  }

  test("OneHotPriorityGate") {
    config.doVerify(new Component {
      val dut = FormalDut(OneHotPriorityGate(prioWidth, width))
      formalSetup(
        dut.io.signals,
        dut.io.priorities,
        dut.io.masked
      )
      assumeAllDifferent(dut.io.priorities)
      dut.io.priorities.map(p => assume(p <= (width - 1)))
    })
  }

  test("OneHotPriorityGateErrorRepro") {
    assume(condition = false) // ignore this test
    config.doVerify(new Component {
      val dut = FormalDut(OneHotPriorityGateErrorRepro(prioWidth, width))
      formalSetup(
        dut.io.signals,
        dut.io.priorities,
        dut.io.masked
      )
      assumeAllDifferent(dut.io.priorities)
      dut.io.priorities.map(p => assume(p <= (width - 1)))
    })
  }

  test("EqualityPriorityGate") {
    config.doVerify(new Component {
      val dut = FormalDut(EqualityPriorityGate(prioWidth, width))
      formalSetup(
        dut.io.signals,
        dut.io.priorities,
        dut.io.masked
      )
      assumeAllDifferent(dut.io.priorities)
    })
  }

  test("EqualityPriorityGate non-unique") {
    config.doVerify(new Component {
      val dut = FormalDut(EqualityPriorityGate(prioWidth, width, allowNonUniquePriorities = true))
      formalSetup(
        dut.io.signals,
        dut.io.priorities,
        dut.io.masked
      )
    })
  }

  def assumeAllDifferent[T <: BitVector](v: Vec[T]): Unit = {
    for (i <- v.indices) {
      for (j <- (i + 1) until v.size) {
        assume(v(i) =/= v(j))
      }
    }
  }

  def formalSetup(
      signals: Bits,
      priorities: Vec[UInt],
      result: Bits,
      anyResult: Option[Bool] = None,
      activeResult: Option[UInt] = None
  ): Unit = new Area {
    val priorityWidth = priorities(0).getWidth
    assumeInitial(ClockDomain.current.readResetWire)

    anyseq(signals)
    anyseq(priorities)

    // create vector to be able to get signalled input with highest priority via finding the max
    val combined = Vec
      .tabulate(signals.getWidth) { i =>
        (signals(i) ## priorities(i) ## U(i, priorityWidth bit)).asUInt
      }
    val best = combined.reduceBalancedTree((a, b) => (a > b) ? a | b)
    val chosenIndex = best.takeLow(priorityWidth).asUInt
    val noneActive = best.msb === False

    // either no signals gets through or the one chosen bit needs to be set
    assert(noneActive || signals(chosenIndex) === True)

    // onehot if any is set, all 0 otherwise
    assert(popcnt(result) === best.msb.asUInt)
    anyResult.map(a => assert(a =/= noneActive))
    activeResult.map(r => assert(noneActive | (r === priorities(chosenIndex))))

    // low prio wins
    cover(
      priorities(0) === 0 && priorities(4) === 4 && result(0) === True
    )
    // high prio wins
    cover(
      priorities(0) === 0 && priorities(4) === 4 && result(4) === True && signals(0) === True
    )
    // prio can win over all bits set, let solver choose priorities(3)
    cover(
      priorities(0) === 1 && priorities(1) === 2 &&
        priorities(2) === 3 && priorities(4) === 0 &&
        result(3) === True && signals === B"11111"
    )
  }
}

class OneHotPriorityGateTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(OneHotPriorityGate(3, 5))

  test(dut, "simple") { dut =>
    dut.io.priorities(0) #= 0
    dut.io.priorities(1) #= 1
    dut.io.priorities(2) #= 2
    dut.io.priorities(3) #= 3
    dut.io.priorities(4) #= 4
    dut.io.signals #= 0x1f
    sleep(20)
    assert(dut.io.masked.toBigInt == 0x10)
  }
}
