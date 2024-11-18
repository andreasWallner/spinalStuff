package andreasWallner

import andreasWallner.sim.PimpedSpinalSimConfig
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import spinal.core.Component
import spinal.core.sim.{SimCompiled, SpinalSimConfig}

import scala.collection.mutable
import scala.util.Random

class NamedSimConfig(val className: String) extends SpinalSimConfig {
  private val ext = mutable.ArrayBuffer[String]()
  private def update() = {
    _workspaceName = className + "/" + ext.prepended("build").mkString("/")
    _testPath = _workspacePath + "/" + className
    //_waveFilePrefix = ext.mkString(".") + ".$TEST"
  }
  update()

  sys.env.getOrElse("SPINALSIM_WAVE", "fst") match {
    case "vcd" => this.withVcdWave
    case "fst" => this.withFstWave
    case "fsdb" => this.withFsdbWave
    case "none" =>
    case x => throw new Exception(s"invalid wave format $x")
  }

  def extendName(s: String) = {
    ext.append(s)
    update()
    this
  }
}

/**
  * A suite of tests running digital simulation
  *
  * Recommended usage:
  *
  * {{{
  * class SomeTests extends HwFunSuite {
  * val dut = namedSimConfig.compile(SomeComponent())
  *
  * test(dut, "my test description") { dut =>
  * ...
  * }
  *
  * // specifying a seed
  * test(dut, "my other test", seed=24) { dut =>
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
 *
 * `extendName` can be used to put simulation into subfolder, which is
 * intended for tests that parameterize (multiple) DUTs:
 *
 * {{{
 * ...
 * val dutName = "$some_$parameter"
 * val dut = namedSimConfig.extendName(dutName).compile(SomeComponent(some, parameter))
 *
 * test(dut, s"parametric_test_$dutName") { dut =>
 * ...
 * }
 * }}}
  */
class SpinalFunSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  var _globalSeed: Option[Int] = None
  def namedSimConfig = new NamedSimConfig(this.getClass.getName)

  override def beforeAll(configMap: ConfigMap): Unit = {
    _globalSeed = configMap
      .getOptional[java.lang.String]("seed")
      .map(s => java.lang.Integer.parseInt(s))
  }

  def test[T <: Component](dut: => SimCompiled[T], testName: String)(
      body: T => Unit
  ): Unit = {
    test(dut, testName, Random.nextInt())(body)
  }

  def test[T <: Component](dut: => SimCompiled[T], testName: String, seed: Int)(
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
