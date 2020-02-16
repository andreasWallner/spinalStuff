package uhu01.basics

import spinal.core._
import spinal.lib._

class Pwm(width : Int) extends Component {
  val io = new Bundle {
    val level = in UInt(width bits)
    val pwm = out Bool
  }
  val counter = Reg(UInt(width bits)) init(0)

  when(counter === (1 << width) - 2) {
    counter := 0
  } otherwise {
    counter := counter + 1
  }
  io.pwm := counter < io.level
}