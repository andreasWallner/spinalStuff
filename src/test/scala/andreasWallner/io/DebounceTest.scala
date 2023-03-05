package andreasWallner.io

import spinal.core.sim._

import andreasWallner.SpinalFunSuite
import andreasWallner.sim._
import org.scalactic.TimesOnInt.convertIntToRepeater

import scala.language.postfixOps
import scala.util.Random

class DebounceTest extends SpinalFunSuite {
  val dut = SpinalSimConfig()
    .withWaveOverride("fst")
    .compile(Debounce(20))

  test(dut, "simple") { dut =>
    SimTimeout((2*10) * 5*14*20)

    dut.clockDomain.forkStimulus(10)
    dut.io.i #= false

    // allow for unreset FFs to settle
    dut.clockDomain.waitSampling(30)

    5 times {
      val target = !dut.io.i.toBoolean
      val toggle = fork {
        (Random.nextInt(6) * 2) + 1 times { // odd number so that we end at a toggled state
          dut.io.i #= !dut.io.i.toBoolean
          dut.clockDomain.waitSampling(20)
        }
      }
      dut.clockDomain.waitSampling(2)
      assert(dut.io.o.toBoolean == target)

      waitUntil(toggle.done || dut.io.o.toBoolean != target)
      assert(dut.io.o.toBoolean == target)

      dut.clockDomain.waitSampling(20)
    }
  }
}
