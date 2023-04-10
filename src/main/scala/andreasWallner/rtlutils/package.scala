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
  }

  class PullToTopTag(val name: String = null) extends SpinalTag {
    override def allowMultipleInstance = false
  }
  implicit class DataPimper(d: Data) {
    def markToPullToTop(name: String = null) = {
      d.addTag(new PullToTopTag(name))
    }
  }

  object PullToTop {
    def apply[T <: Component](c: T) = {
      c.rework {
        // we might stumble multiple times over one signal (e.g. because of assignments in different
        // conditional contexts, deduplicate here
        val toPull = mutable.Set[Data with SpinalTagReady]()
        c.walkComponents(cc =>
          cc.dslBody.walkStatements(s =>
            s.walkDrivingExpressions {
              case tr: Data with SpinalTagReady if tr.hasTag(classOf[PullToTopTag]) => toPull += tr
              case _                                                                =>
            }
          )
        )

        for (tp <- toPull) {
          val tag = tp.getTag(classOf[PullToTopTag])
          val pulled = tp.pull()
          val topPort = cloneOf(pulled).asOutput()
          topPort := pulled
          if (tag.get.name != null)
            topPort.setName(tag.get.name)
          else
            topPort.setWeakName(f"pulled_${tp.component.getPath("_")}_${tp.getName()}")
        }
      }
      c
    }
  }
}
