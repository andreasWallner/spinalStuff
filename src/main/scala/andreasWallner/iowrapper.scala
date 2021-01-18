package andreasWallner

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import scala.collection.mutable

case class IOBUF() extends BlackBox {
  val I, T = in Bool()
  val O = out Bool()
  val IO = inout(Analog(Bool))
}

object XilinxInOutWrapper {
  def apply[T <: Component](c : T) : T = {
    val dataParents = mutable.LinkedHashMap[Data,Int]()
    def add(that : Data): Unit ={
      if(that.parent != null){
        dataParents(that.parent) = dataParents.getOrElseUpdate(that.parent,0) + 1
        add(that.parent)
      }
    }
    for(io <- c.getAllIo){
      add(io)
    }

    c.rework {
      for ((dataParent, count) <- dataParents) {
        dataParent match {
          case bundle: TriState[_]  if bundle.writeEnable.isOutput  => {
            val name = bundle.getName()
            val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
            println(name)
            val bufs = Array.fill[IOBUF](bundle.dataType.getBitsWidth)(IOBUF())
            val ios = Analog(Bits(bundle.dataType.getBitsWidth bits))
            newIo.assignFromBits(ios)
            bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
            (bufs.zipWithIndex zip bundle.write.asBits.asBools) map {
              case ((buf, idx), w) =>
                buf.setWeakName("iOBUF_" + name + "_" + idx)
                buf.I := w
                buf.T := bundle.writeEnable
                bundle.read.assignFromBits(buf.O.asBits, idx, 1 bits)
                ios(idx) := buf.IO// ### <- doesn't do the right thing if _ is not Bits
            }
          }
          case bundle : TriStateOutput[_] if bundle.isOutput => {
            val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
            bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
            when(bundle.writeEnable){
              newIo := bundle.write
            }
          }
          case bundle: ReadableOpenDrain[_]  if bundle.isMasterInterface => {
            val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
            bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
            bundle.read.assignFrom(newIo)
            for((value, id) <- bundle.write.asBits.asBools.zipWithIndex) {
              when(!value){
                newIo.assignFromBits(B"0", id, 1 bits)
              }
            }
//            for(bt <- bundle.write.flatten){
//              for((value, id) <- bt.asBits.asBools.zipWithIndex) {
//                when(!value){
//                  bt.assignFromBits("0", id, 1 bits)
//                }
//              }
//            }
          }
          case bundle: TriStateArray if bundle.writeEnable.isOutput => {
            val newIo = inout(Analog(bundle.write)).setWeakName(bundle.getName())
            bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
            bundle.read.assignFrom(newIo)
            for(i <- 0 until bundle.width) {
              when(bundle.writeEnable(i)) {
                newIo(i) := bundle.write(i)
              }
            }
          }
          case _ =>
        }
      }
    }
    c
  }
}

case class Foo() extends Bundle {
  val three = Bits(3 bit)
  val five = Bits(5 bit)
}

case class Test() extends Component {
  val io = new Bundle {
    //val tri_bits = master(TriState(Bits(2 bit)))
    val tri_bool = master(TriState(Bool))
    //val tri_foo = master(TriState(Foo()))

    //val r_bits = out Bits(2 bit)
    //val w_bits = in Bits(2 bit)
    val r_bool = out Bool
    val w_bool = in Bool
    val we = in Bool()
  }
  /*io.tri_bits.write := io.w_bits
  io.r_bits := io.tri_bits.read
  io.tri_bits.writeEnable := io.we
*/
  io.tri_bool.write := io.w_bool
  io.r_bool := io.tri_bool.read
  io.tri_bool.writeEnable := io.we
/*
  io.tri_foo.write.five.setAll()
  io.tri_foo.write.three.clearAll()
  io.tri_foo.writeEnable := io.we*/
}

object TestComponent {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(XilinxInOutWrapper(Test()))
  }
}
