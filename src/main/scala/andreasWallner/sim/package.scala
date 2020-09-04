package andreasWallner

import spinal.core._
import spinal.core.sim._
import spinal.lib.io.TriState
import scala.collection.mutable

class SimDriverState() {
  var driveEnable = false
  var driveLevel = false

  override def toString(): String =
    return "[SimDriverState enable=" + driveEnable + " level=" + driveLevel + "]"
}

object DriverStates {
  val m = mutable.Map[TriState[Bool], SimDriverState]()
}

package object sim {
  implicit class SimTriStatePimper(tri: TriState[Bool]) {
    def simulatePullup() = {
      val state = new SimDriverState
      DriverStates.m += (tri -> state)
      println(state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          "drivers may not be active concurrently"
        )
        val newState =
          !((state.driveEnable && !state.driveLevel) || (tri.writeEnable.toBoolean && !tri.write.toBoolean))
        if (newState != tri.read.toBoolean) {
          tri.read #= newState
        }
      }
    }

    def drive(level: Boolean) = {
      assert(DriverStates.m contains tri, "no simulation set up for TriState")
      val state = DriverStates.m(tri)
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz() = {
      assert(DriverStates.m contains tri, "no simulation set up for TriState")
      val state = DriverStates.m(tri)
      state.driveEnable = false
    }
  }
}
