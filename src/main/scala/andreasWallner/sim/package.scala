package andreasWallner

import spinal.core._
import spinal.core.sim._
import spinal.lib.io.TriState
import spinal.sim.SimManagerContext

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

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
  val bitVectorStates =
    mutable.Map[TriState[BitVector], SimDriverStateBitVector]()
}

package object sim {
  object simLog {
    def apply(xs: Any*): Unit = {
      println(
        f"[${Console.BLUE}${simTime()}%9d${Console.RESET}] " + xs
          .map(_.toString)
          .mkString(" ")
      )
    }
  }

  /** calculate number of simulation cycles for given frequency */
  def simCycles(f: HertzNumber) =
    (f.toTime / TimeNumber(SimManagerContext.current.manager.timePrecision)).toLong

  implicit class SimTriStatePimperBitVector[T <: BitVector](tri: TriState[T]) {
    def simulatePullup(readDelay: Int = 0): Unit = {
      val state = new SimDriverStateBitVector()
      DriverStates.bitVectorStates += (tri
        .asInstanceOf[TriState[BitVector]] -> state)

      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean && (state.driveLevel != tri.write.toBigInt)),
          f"${simTime()} drivers must not drive against each other (testbench drives ${state.driveLevel}%x, dut drives ${tri.write.toBigInt}%x)"
        )
        val newState =
          if (state.driveEnable) state.driveLevel
          else if (tri.writeEnable.toBoolean) tri.write.toBigInt
          else BigInt(0)
        if (readDelay == 0) {
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
      assert(
        DriverStates.bitVectorStates contains tri
          .asInstanceOf[TriState[BitVector]],
        "no simulation set up for TriState"
      )
      val state =
        DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          f"${simTime()} drivers may not be active concurrently"
        )
      }
    }

    def drive(level: Int): Unit = drive(BigInt.int2bigInt(level))
    def drive(level: Long): Unit = drive(BigInt.long2bigInt(level))
    def drive(level: BigInt): Unit = {
      assert(
        DriverStates.bitVectorStates contains tri
          .asInstanceOf[TriState[BitVector]],
        "no simulation set up for TriState"
      )
      val state =
        DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz(): Unit = {
      assert(
        DriverStates.bitVectorStates contains tri
          .asInstanceOf[TriState[BitVector]],
        "no simulation set up for TriState"
      )
      val state =
        DriverStates.bitVectorStates(tri.asInstanceOf[TriState[BitVector]])
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
        if (readDelay == 0) {
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
      assert(
        DriverStates.boolStates contains tri,
        "no simulation set up for TriState"
      )
      val state = DriverStates.boolStates(tri)
      forkSensitive {
        assert(
          !(state.driveEnable && tri.writeEnable.toBoolean),
          f"${simTime()} drivers may not be active concurrently"
        )
      }
    }

    def drive(level: Boolean): Unit = {
      assert(
        DriverStates.boolStates contains tri,
        "no simulation set up for TriState"
      )
      val state = DriverStates.boolStates(tri)
      state.driveEnable = true
      state.driveLevel = level
    }

    def highz(): Unit = {
      assert(
        DriverStates.boolStates contains tri,
        "no simulation set up for TriState"
      )
      val state = DriverStates.boolStates(tri)
      state.driveEnable = false
    }

    def isOpenDrain: Boolean = !tri.writeEnable.toBoolean || (tri.writeEnable.toBoolean && !tri.write.toBoolean)
    def isPushPull: Boolean = tri.writeEnable.toBoolean
  }

  implicit class SimBool(b: Bool) {
    def strobe(v: Boolean, cd: ClockDomain): Unit = {
      fork {
        cd.waitActiveEdge()
        b #= v
        cd.waitActiveEdge()
        b #= !v
      }
    }

    def strobe(cd: ClockDomain): Unit = {
      fork {
        cd.waitActiveEdge()
        b #= !b.toBoolean
        cd.waitActiveEdge()
        b #= !b.toBoolean
      }
    }

    def strobe(): Unit = strobe(b.component.clockDomain)
    def strobe(v: Boolean): Unit = strobe(v, b.component.clockDomain)
  }

  implicit class PimpedSimBitVector(bv: BitVector) {
    def changedInt(): Int = {
      val width = bv.getWidth
      assert(width < 32)
      val current = bv.toInt

      @tailrec
      def changedValue(): Int = {
        val candidate = Random.nextInt() & ((1 << width) - 1)
        if (candidate != current) candidate else changedValue()
      }

      val next = changedValue()
      bv #= next
      next
    }

    def changedLong(): Long = {
      val width = bv.getWidth
      assert(width < 64)
      val current = bv.toInt

      @tailrec
      def changedValue(): Long = {
        val candidate = Random.nextLong() & ((1L << width) - 1)
        if (candidate != current) candidate else changedValue()
      }

      val next = changedValue()
      bv #= next
      next
    }

    def changedBigInt(): BigInt = {
      val width = bv.getWidth
      val current = bv.toBigInt

      @tailrec
      def changedValue(): BigInt = {
        val candidate = BigInt(width, Random)
        if (candidate != current) candidate else changedValue()
      }

      val next = changedValue()
      bv #= next
      next
    }

    def changed(): BigInt = {
      bv.getWidth match {
        case x if x < 32 => changedInt()
        case x if x < 64 => changedLong()
        case _           => changedBigInt()
      }
    }
  }

  implicit class PimpedSpinalSimConfig(config: SpinalSimConfig) {
    // TODO use WaveFormat
    def withWaveOverride(wave: String = null): SpinalSimConfig = {
      sys.env.getOrElse("SPINALSIM_WAVE", wave) match {
        case "vcd"  => config.withVcdWave
        case "fst"  => config.withFstWave
        case "fsdb" => config.withFsdbWave
        case null   => config
        case _      => throw new Exception(s"invalid wave format $wave")
      }
    }
    def withWorkspacePathOverride(path: String = null): SpinalSimConfig = {
      sys.env.getOrElse("SPINALSIM_WORKSPACE", path) match {
        case null => config
        case _    => config.workspacePath(path)
      }
    }
  }

  /**
   * A string that can be added to a component and written during simulation
   *
   * {{{
   *   val myString = SimString("debug_string")
   *   val dut = SimConfig.compile { MyComponent().add(simString) }
   *   dut.doSim { dut =>
   *     myString #= "something to log to simulation"
   *   }
   * }}}
   * @param name name of the signal added to component
   * @param length reserved length, max length of string to log
   */
  case class SimString(name: String, length: Int = 20) {
    private var reference: Option[Bits] = None
// TODO check if there is a current scope with a component, if so register automatically
    def #=(s: String): Unit = {
      assert(reference.isDefined, s"SimString '$name' has not been added to any component before use")
      var toAssignBI = BigInt(0)
      s.take(20).padTo(20, ' ').foreach(c => toAssignBI = (toAssignBI << 8) + c)
      reference.get #= toAssignBI
    }

    def #=(bi: BigInt): Unit = this.#=("0x" + bi.toString(16))

    def #=(i: Int): Unit = this.#=(BigInt(i))

    def #=(l: Long): Unit = this.#=(BigInt(l))

    def setRef(b: Bits): Unit = {
      assert(reference.isEmpty, s"SimString '$name' can't be added to a component twice")
      reference = Some(b)
    }

    def materialize() = {
      val signal = Reg(Bits(length * 8 bit)) init B(0x20, 8 bit) #* length
      setRef(signal)
      signal.simPublic()
      signal.dontSimplifyIt()
      signal.setName(name)
      this
    }
  }

  /**
   * Used to add `SimString`s to a component @see SimString
   */
  implicit class ComponentSimStringPimper[T <: Component](c: T) {
    def add(ss: SimString): T = {
      c.rework { ss.materialize() }
      c
    }
    def add(ss: SimString*): T = {
      ss.foreach{add}
      c
    }
    def add(ss: IterableOnce[SimString]): T = {
      ss.iterator.foreach{add}
      c
    }
  }

  // WIP sim slicing for BitVector
  object SimBitVector {
    val values = mutable.HashMap[BitVector, BigInt]()
  }
  class SimBitVector(d: BitVector, slice: Range) {
    val mask = ((BigInt(1) << slice.length) - 1) << slice.min

    def #=(b: BigInt): Unit = {
      val current = SimBitVector.values.getOrElse(d, BigInt(0))
      val updated = (current & mask) | b << slice.min
      SimBitVector.values += ((d, updated))
      d #= updated
    }
  }
}
