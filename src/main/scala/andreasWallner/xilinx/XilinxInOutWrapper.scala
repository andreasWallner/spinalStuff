package andreasWallner.xilinx

import spinal.core._
import spinal.core.fiber.Engine
import spinal.lib.blackbox.xilinx.s7.IOBUF
import spinal.lib.io._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

object XilinxInOutWrapper {
  def apply[T <: Component](c: T): T = {
    Engine.get.onCompletion += (() => {
      val dataParents = mutable.LinkedHashMap[Data, Int]()
      @tailrec
      def add(that: Data): Unit = {
        if (that.parent != null) {
          dataParents(that.parent) = dataParents.getOrElseUpdate(that.parent, 0) + 1
          add(that.parent)
        }
      }
      for (io <- c.getAllIo) {
        add(io)
      }

      def flattenedName(bundle: Data, signal: Data, marker: String) =
        bundle.getName() + signal
          .getName()
          .substring(bundle.getName().length + marker.length)

      def makeBuffers(width: Int, name: String) = {
        val buffers = Array.fill[IOBUF](width)(IOBUF())
        val t = Bits(width bit)
        val i = Bits(width bit)
        val o = Bits(width bit)
        val io = inout(Analog(Bits(width bit))).setWeakName(name)
        buffers.zipWithIndex foreach {
          case (b, idx) =>
            b.setWeakName("IOBUF_" + name + "_" + idx)
            b.T := t(idx)
            b.I := i(idx)
            o(idx) := b.O
            io(idx) := b.IO
        }
        (t, i, o, io)
      }

      c.rework {
        for ((dataParent, count) <- dataParents) {
          dataParent match {
            case bundle: TriState[_] if bundle.writeEnable.isOutput =>
              (bundle.write.flatten zip bundle.read.flatten).foreach {
                case (dw: Data, dr: Data) =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (t, i, o, _) = makeBuffers(widthOf(dw), name)
                  t.setAllTo(!bundle.writeEnable)
                  i := dw.asBits
                  dr.assignFromBits(o)
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: TriStateOutput[_] if bundle.isOutput =>
              bundle.write.flatten.foreach {
                dw: Data =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (t, i, _, _) = makeBuffers(widthOf(dw), name)
                  t.setAllTo(!bundle.writeEnable)
                  i := dw.asBits
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: ReadableOpenDrain[_] if bundle.isMasterInterface =>
              (bundle.write.flatten zip bundle.read.flatten).foreach {
                case (dw: Data, dr: Data) =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (t, i, o, _) = makeBuffers(widthOf(dw), name)
                  t := dw.asBits
                  i.clearAll()
                  dr.assignFromBits(o)
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: TriStateArray if bundle.writeEnable.isOutput =>
              val name = bundle.getName()
              val (t, i, o, _) = makeBuffers(bundle.width, name)
              t := ~bundle.writeEnable
              i := bundle.write
              bundle.read := o
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case _ =>
          }
        }
      }
    })
    c
  }
}
