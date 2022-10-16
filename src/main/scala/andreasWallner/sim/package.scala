package andreasWallner

import spinal.core._
import spinal.core.sim._
import spinal.lib.io.TriState
import scala.collection.mutable

class SimDriverState() {
  var driveEnable = false
  var driveLevel = false

  override def toString: String =
    f"[SimDriverState enable=$driveEnable level=$driveLevel]"
}

class SimDriverStateBitVector() {
  var driveEnable = false
  var driveLevel = BigInt.int2bigInt(0)

  override def toString: String =
    s"[SimDriverState enable=$driveEnable level=${driveLevel.toString(16)}]"
}

object DriverStates {
  val boolStates = mutable.Map[TriState[Bool], SimDriverState]()
  val bitVectorStates = mutable.Map[TriState[BitVector], SimDriverStateBitVector]()
}

package object sim {
  object simLog {
    def apply(s: String): Unit = {
      println(f"[${Console.BLUE}${simTime()}${Console.RESET}] $s")
    }
  }

  implicit class SimTriStatePimperBitVector[T <: BitVector](tri: TriState[T]) {
    def simulatePullup(readDelay:Int = 0): Unit = {
      val state = new SimDriverStateBitVector()
      DriverStates.bitVectorStates += (tri.asInstanceOf[TriState[BitVector]] -> state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean && (state.driveLevel != tri.write.toBigInt)),
          f"${simTime()} drivers must not drive against each other (testbench drives ${state.driveLevel}%x, dut drives ${tri.write.toBigInt}%x)"
        )
        val newState = if(state.driveEnable) state.driveLevel
        else if(tri.writeEnable.toBoolean) tri.write.toBigInt
        else BigInt(0)
        if(readDelay == 0) {
          if (newState != tri.read.toBigInt)
            tri.read #= newState
        } else {
          delayed(readDelay) {
            if (newState != tri.read.toBigInt)
              tri.read #= newState
          }
        }
      }
    }

    def prohibitAnyConcurrentDrivers(): Unit = {
      assert(DriverStates.bitVectorStates contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
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
      assert(DriverStates.bitVectorStates contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz(): Unit = {
      assert(DriverStates.bitVectorStates contains tri.asInstanceOf[TriState[BitVector]], "no simulation set up for TriState")
      val state = DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
      state.driveEnable = false
    }
  }

  implicit class SimTriStatePimper(tri: TriState[Bool]) {
    def simulatePullup(readDelay: Int = 0): Unit = {
      val state = new SimDriverState
      DriverStates.boolStates += (tri -> state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean && (state.driveLevel ^ tri.write.toBoolean)),
          s"${simTime()} drivers must not drive against each other (testbench drives ${state.driveLevel}, dut drives ${tri.write.toBoolean})"
        )
        val newState =
          !((state.driveEnable && !state.driveLevel) || (tri.writeEnable.toBoolean && !tri.write.toBoolean))
        if(readDelay == 0) {
          if (newState != tri.read.toBoolean) {
            tri.read #= newState
          }
        } else {
          delayed(readDelay) {
            if (newState != tri.read.toBoolean) {
              tri.read #= newState
            }
          }
        }
      }
    }

    def prohibitAnyConcurrentDrivers(): Unit = {
      assert(DriverStates.boolStates contains tri, "no simulation set up for TriState")
      val state = DriverStates.boolStates(tri)
      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          f"${simTime()} drivers may not be active concurrently"
        )
      }
    }

    def drive(level: Boolean): Unit = {
      assert(DriverStates.boolStates contains tri, "no simulation set up for TriState")
      val state = DriverStates.boolStates(tri)
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz(): Unit = {
      assert(DriverStates.boolStates contains tri, "no simulation set up for TriState")
      val state = DriverStates.boolStates(tri)
      state.driveEnable = false
    }
  }

  implicit class SimBool(b: Bool) {
    def strobe(v: Boolean, cd: ClockDomain):Unit = {
      fork {
        cd.waitActiveEdge()
        b #= v
        cd.waitActiveEdge()
        b #= !v
      }
    }

    def strobe(cd: ClockDomain):Unit = {
      fork {
        cd.waitActiveEdge()
        b #= !b.toBoolean
        cd.waitActiveEdge()
        b #= !b.toBoolean
      }
    }
  }
}
