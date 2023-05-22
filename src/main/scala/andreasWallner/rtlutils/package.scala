package andreasWallner

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps

package object rtlutils {
  implicit class ClockDomainPimper(cd: ClockDomain) {

    /**
      * Generate a ClockDomain using a given enable signal derived from the current ClockDomain
      */
    def withEnable(
        enable: Bool,
        clockEnableDivisionRate: ClockDomain.DivisionRate = ClockDomain.UnknownDivisionRate()
    ): ClockDomain = {
      assert(
        !cd.hasClockEnableSignal,
        "currently not implemented for ClockDomains that already have an enable"
      )
      // ensure the derived domain is reset even if enable stays low
      val enableOrReset = cd.config.resetKind match {
        case `SYNC` if cd.hasResetSignal => enable || cd.isResetActive
        case _                           => enable
      }
      val newCd = cd.copy(
        clockEnable = enableOrReset,
        clockEnableDivisionRate = clockEnableDivisionRate,
        config = cd.config.copy(clockEnableActiveLevel = HIGH)
      )
      newCd.setSyncWith(cd)
    }
  }

  /** Convert vector of streams to wide signal vectors grouping valid/ready/data
    *
    * {{{
    *   val io = new Bundle {
    *     val streams = Vec(slave(Stream(Bits(5 bit))), 4)
    *     val grouped = master(GroupedStreams(streams))
    *   }
    *   io.grouped.driveFrom(io.streams)
    * }}}
   **/
  case class GroupedStreams[T <: BitVector](streams: Vec[Stream[T]])
      extends Bundle
      with IMasterSlave {
    val valids = Bits(streams.size bits)
    val readies = Bits(streams.size bits)
    val payloads = Bits(streams.map(_.payload.getWidth).sum bit)

    def asMaster(): Unit = {
      out(valids, payloads)
      in(readies)
    }

    def driveFrom(streams: Vec[Stream[T]]): Unit = {
      streams.zipWithIndex.foreach {
        case (stream, idx) =>
          valids(idx) <> stream.valid
          readies(idx) <> stream.ready
      }
      payloads <> Cat(streams.map(_.payload))
    }
  }

  implicit class ComponentPimper(c: Component) {
    def getAllClockDomains(): Set[ClockDomain] = {
      val clockDomains = mutable.LinkedHashSet[ClockDomain]()
      c.walkComponents(c => c.dslBody.walkStatements(s => s.foreachClockDomain(clockDomains.add)))
      clockDomains.toSet
    }

    def getFastestClock() =
      c.getAllClockDomains()
        .map(cd => cd.frequency.getValue)
        .max(Ordering.by[HertzNumber, BigDecimal](hn => hn.toBigDecimal))
  }

  implicit class DataPimper(d: Data) {
    def markToPullToTop(name: String = null) = {
      PullToTop.toPull += ((d, name))
      d
    }
  }

  /** Pull all marked signal to the toplevel
    *
    * {{{
    * // somewhere in a component
    * somesignal.markToPullToTop()
    *
    * // for generation
    * val report = SpinalVerilog(PullToTop(MyComponent()))
    * }}}
    */
  object PullToTop {
    val toPull = mutable.Set[(Data, String)]()
    def apply[T <: Component](c: T, filter: ((Data, String)) => Boolean = _ => true) = {
      c.rework {
        for ((tp, name) <- toPull.filter(filter)) {
          val pulled = tp.pull()
          val topPort = cloneOf(pulled).asOutput()
          topPort := pulled
          if (name != null)
            topPort.setName(name)
          else
            topPort.setWeakName(f"probe_${tp.component.getPath("_")}_${tp.getName()}")
        }
      }
      c
    }
  }
}
