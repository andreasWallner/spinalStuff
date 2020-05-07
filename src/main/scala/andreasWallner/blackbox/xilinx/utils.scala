package andreasWallner.blackbox.xilinx

import spinal.core._
import spinal.lib._

object ResetBridge {
  def on(
      resets: Array[Bool],
      clk: Bool,
      flopCnt: Int = 2,
      resetPolarity: Polarity = HIGH
  ): Bool = {
    val rb = ResetBridge(resets.size, flopCnt, resetPolarity)
    rb.io.areset.asBools.zip(resets).foreach({ case (a, b) => a := b })
    rb.io.clk := clk
    rb.io.reset
  }
}

case class ResetBridge(
    lines: Int,
    flopCnt: Int = 2,
    resetPolarity: Polarity = HIGH
) extends Component {
  val io = new Bundle {
    val areset = in Bits (lines bits)
    val clk = in Bool
    val reset = out Bool
  }
  assert(
    flopCnt >= 2,
    "at least 2 flipflops are needed for reset synchronization"
  )

  assert(flopCnt == 2, "not yet implemented") // TODO
  val anyReset = io.areset.orR

  def ff(D: Bool): Bool = {
    if (resetPolarity == HIGH)
      FDP(C = io.clk, D = D, PRE = anyReset)
    else
      FDC(C = io.clk, D = D, CLR = anyReset)
  }

  val sync0 = ff(if (resetPolarity == HIGH) False else True)
  io.reset := ff(sync0)
}
