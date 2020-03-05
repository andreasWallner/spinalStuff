package innovative_solutions.basics

import spinal.core._
import spinal.lib._

case class PwmGenerics(counterWidth: Int = 32, channelCnt: Int = 1) {}

case class Pwm(generics: PwmGenerics) extends Component {
  val io = new Bundle {
    val max_count = in UInt (generics.counterWidth bits)
    val levels = in Vec (UInt(generics.counterWidth bits), generics.channelCnt)
    val pwms = out Vec (Bool, generics.channelCnt)
  }
  val counter = Reg(UInt(generics.counterWidth bits)) init (0)
  val levelsLatched = Vec(Reg(UInt(generics.counterWidth bits)), generics.channelCnt)
  val counter_next = UInt(generics.counterWidth bits)
  //val pwmsRegistered = Vec(Reg(Bool), generics.channelCnt)

  
  //io.pwms := pwmsRegistered

  when(counter === io.max_count) {
    counter_next := 0
  } otherwise {
    counter_next := counter + 1
  }

  // TODO: registered outputs
  // TODO: enable & run
  // TODO: count modes
  // TODO: output modes

  when(counter === io.max_count) {
    levelsLatched := io.levels
  }
  counter := counter_next

  for ((pwmRegistered, levelLatched) <- io.pwms.zip(levelsLatched))
    pwmRegistered := counter < levelLatched
}
