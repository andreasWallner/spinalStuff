package andreasWallner.basics

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
  levelsLatched.foreach((l) => l init(0))
  
  val pwmsNext = Vec(Bool, generics.channelCnt)
  val pwmsRegs = RegNext(pwmsNext)
  pwmsRegs.foreach((p) => p init(False))
  io.pwms := pwmsRegs

  val counter_next = UInt(generics.counterWidth bits)
  when(counter === io.max_count) {
    counter_next := 0
  } otherwise {
    counter_next := counter + 1
  }

  // TODO: enable & run
  // TODO: count modes
  // TODO: output modes

  when(counter === io.max_count) {
    levelsLatched := io.levels
  }
  counter := counter_next

  for ((pwmNext, levelLatched) <- pwmsNext.zip(levelsLatched))
    pwmNext := counter < levelLatched
}
