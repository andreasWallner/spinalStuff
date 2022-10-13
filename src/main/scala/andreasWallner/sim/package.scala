package andreasWallner

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.io.TriState
import scala.collection.mutable

class SimDriverState() {
  var driveEnable = false
  var driveLevel = false

  override def toString(): String =
    "[SimDriverState enable=" + driveEnable + " level=" + driveLevel + "]"
}

class SimDriverStateBitVector(width: Int) {
  var driveEnable = false
  var driveLevel = BigInt.int2bigInt(0)

  override def toString(): String =
    f"[SimDriverState enable=${driveEnable} level=${driveLevel.toString}]" // TODO format some
}

object DriverStates {
  val m = mutable.Map[TriState[Bool], SimDriverState]()
  val d = mutable.Map[TriState[BitVector], SimDriverStateBitVector]()
}

package object sim {
  object simLog {
    def apply(s: String) = {
      println(f"[${Console.BLUE}${simTime()}${Console.RESET}] $s")
    }
  }

  implicit class SimTriStatePimperBitVector[T <: BitVector](tri: TriState[T]) {
    def simulatePullup() = {
      val state = new SimDriverStateBitVector(widthOf(tri.write))
      DriverStates.d += (tri.asInstanceOf[TriState[BitVector]] -> state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean && (state.driveLevel != tri.write.toBigInt)),
          f"${simTime()} concurrent drivers may not drive mismatching values"
        )
        val newState = if(state.driveEnable) state.driveLevel
        else if(tri.writeEnable.toBoolean) tri.write.toBigInt
        else BigInt(0)
        if (newState != tri.read.toBigInt)
          tri.read #= newState
      }
    }

    def prohibitAnyConcurrentDrivers() = {
      assert(DriverStates.d contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.d(tri.asInstanceOf[TriState[BitVector]])
      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          f"${simTime()} drivers may not be active concurrently"
        )
      }
    }

    def drive(level: Int):Unit = drive(BigInt.int2bigInt(level))
    def drive(level: Long):Unit = drive(BigInt.long2bigInt(level))
    def drive(level: BigInt):Unit = {
      assert(DriverStates.d contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.d(tri.asInstanceOf[TriState[BitVector]])
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz() = {
      assert(DriverStates.d contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.d(tri.asInstanceOf[TriState[BitVector]])
      state.driveEnable = false
    }
  }

  implicit class SimTriStatePimper(tri: TriState[Bool]) {
    def simulatePullup() = {
      val state = new SimDriverState
      DriverStates.m += (tri -> state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean && (state.driveLevel || tri.write.toBoolean)),
          f"${simTime()} drivers may not be active concurrently"
        )
        val newState =
          !((state.driveEnable && !state.driveLevel) || (tri.writeEnable.toBoolean && !tri.write.toBoolean))
        if (newState != tri.read.toBoolean) {
          tri.read #= newState
        }
      }
    }

    def prohibitAnyConcurrentDrivers() = {
      assert(DriverStates.m contains tri, "no simulation set up for TriState")
      val state = DriverStates.m(tri)
      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          f"${simTime()} drivers may not be active concurrently"
        )
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

  implicit class SimBool(b: Bool) {
    def strobe(v: Boolean, cd: ClockDomain) = {
      fork {
        cd.waitActiveEdge(1)
        b #= v
        cd.waitActiveEdge(1)
        b #= !v
      }
    }
  }
}
