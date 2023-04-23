package andreasWallner

import andreasWallner.sim.PimpedSpinalSimConfig
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import spinal.core.Component
import spinal.core.sim.{SimCompiled, SimConfig}

import scala.util.Random

/**
  * A suite of tests running digital simulation
  *
  * Recommended usage:
  *
  * {{{
  * class SomeTests extends HwFunSuite {
  * val dut = SimConfig.withFstWave.compile(SomeComponent())
  *
  * test(dut, "my test description") { dut =>
  * ...
  * }
  *
  * // specifying a seed
  * test(dut, "my other test", 24) { dut =>
  * ...
  * }
  * }
  * }}}
  *
  * This class allows the SpinalSim seed to be overwritten from the
  * command line: <code>testOnly path.to.the.Class -- -Dseed=1</code>
  *
  * Use `namedSimConfig` as `SimConfig` to use the testsuite name as
  * the workspace name. This way multiple testsuites testing the same
  * DUT component will have consistent folder names.
  */
class SpinalFunSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  var _globalSeed: Option[Int] = None
  // TODO how to deal with parameterized compiles?
  val namedSimConfig = SimConfig.workspaceName(this.getClass.getName).withWaveOverride("fst")

  override def beforeAll(configMap: ConfigMap): Unit = {
    _globalSeed = configMap
      .getOptional[java.lang.String]("seed")
      .map(s => java.lang.Integer.parseInt(s))
  }

  def test[T <: Component](dut: SimCompiled[T], testName: String)(
      body: T => Unit
  ): Unit = {
    test(dut, testName, Random.nextInt())(body)
  }

  def test[T <: Component](dut: SimCompiled[T], testName: String, seed: Int)(
      body: T => Unit
  ): Unit = {
    test(testName) {
      dut.doSim(testName, seed = _globalSeed.getOrElse(seed)) { dut =>
        body(dut)
      }
    }
  }

  def testUntilVoid[T <: Component](dut: SimCompiled[T], testName: String)(
      body: T => Unit
  ): Unit = {
    test(dut, testName, Random.nextInt())(body)
  }

  def testUntilVoid[T <: Component](
      dut: SimCompiled[T],
      testName: String,
      seed: Int
  )(body: T => Unit): Unit = {
    test(testName) {
      dut.doSimUntilVoid(testName, seed = _globalSeed.getOrElse(seed)) { dut =>
        body(dut)
      }
    }
  }
}
