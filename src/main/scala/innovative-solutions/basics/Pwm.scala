package innovative_solutions.basics

import spinal.core._
import spinal.lib._

case class Pwm(width : Int) extends Component {
  val io = new Bundle {
    val max_level = in UInt(width bits)
    val level = in UInt(width bits)
    val pwm = out Bool
  }
  val counter = Reg(UInt(width bits)) init(0)
  val level_safe = Reg(UInt(width bits)) init(0)
  
  when(counter === io.max_level) {
    level_safe := io.level
    counter := 0
  } otherwise {
    counter := counter + 1
  }
  io.pwm := counter < level_safe
}