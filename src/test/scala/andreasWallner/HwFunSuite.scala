package andreasWallner

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import spinal.core.Component
import spinal.core.sim.SimCompiled

import scala.util.Random

/**
 * A suite of tests running digital simulation
 *
 * Recommanded usage:
 *
 * <pre class="stHighlight">
 * class SomeTests extends HwFunSuite {
 *   val dut = SimConfig.withFstWave.compile(SomeComponent())
 *
 *   test(dut, "my test description") { dut =>
 *     ...
 *   }
 *
 *   // specifying a seed
 *   test(dut, "my other test", 24) { dut =>
 *     ...
 *   }
 * }
 * </pre>
 *
 * This class allows the SpinalSim seed to be overwritten from the
 * command line: <code>testOnly path.to.the.Class -- -Dseed=1</code>
 */
class HwFunSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  var _globalSeed: Option[Int] = None
  override def beforeAll(configMap: ConfigMap):Unit = {
    _globalSeed = configMap.getOptional[java.lang.String]("seed").map(s => java.lang.Integer.parseInt(s))
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
      dut.doSim(testName, seed=_globalSeed.getOrElse(seed)) { dut =>
        body(dut)
      }
    }
  }
}
