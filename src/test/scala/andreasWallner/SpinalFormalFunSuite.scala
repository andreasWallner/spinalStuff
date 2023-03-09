package andreasWallner

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.Bool
import spinal.idslplugin.Location

object SpinalFormal extends Tag("andreasWallner.formal")

class SpinalFormalFunSuite extends AnyFunSuite {
  def assert(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assert(assertion)
  }

  def assume(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assume(assertion)
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    test(testName, SpinalFormal) {
      testFun
    }
  }

  def shouldFail(body: => Unit) =
    assert(try {
      body
      false
    } catch {
      case e: Throwable => println(e); true
    })
}
