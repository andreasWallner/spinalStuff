package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7._
import andreasWallner.blackbox.xilinx._
import andreasWallner.test.HsiLoopbackTest

case class Board_2_14() extends Component {
  val io = new Bundle {
    val fxclk = in Bool () // P16 <-> // FIXME
    val ifclk = in Bool () // P17 <-> ifclk_in
    val reset = in Bool () // V16 <-> reset

    val dq = inout(Analog(Bits(16 bits))) // K17, K18... <-> fd
    val slwr_n = out Bool () // U11 <-> SLWR
    val sloe_n = out Bool () // U13 <-> SLOE
    val slrd_n = out Bool () // V12 <-> SLRD
    val flaga = in Bool () // V11 <-> empty_flag
    val flagb = in Bool () // V14 <-> full_flag
    val pktend_n = out Bool () // U12

    //val rdwr_b = Bool() // R16 // TODO dir gpio
    //val csi_b = Bool() // V15 // TODO dir gpio

    val gpio = inout(Analog(Bits(4 bits))) // will pullups
    /*val gpio23 = ? // T14
    val gpio25 = ? // V16
    val gpio26 = ? // U14
    val a0 = ? // U16
    val a1 = ? // T16
    val int_n = ? // T13*/

    /*val scl = ? // T10
    val sda = ? // T9

    val spi = new Bundle { // TODO directions
      val clk = Bool()  // R17
      val cs_n = Bool() // N17
      val miso = Bool() // N16
      val mosi = Bool() // L16
    }*/

    val led1_n = out Bool () // T11 <-> led

    /*val uart = new Bundle { // TODO directions
      val rx = Bool() // V17
      val tx = Bool() // U17
      val rts = Bool() // T16
      val cts = Bool() // U18
    }*/

    val A = inout(Analog (Bits(30 bits))) // K16...
    val B = inout(Analog (Bits(30 bits))) // J18...
    val C = inout(Analog (Bits(30 bits))) // U9...
    val D = inout(Analog (Bits(30 bits))) // V9...
  }

  val mmce = MMCME2_BASE(CLKIN1_PERIOD = 1.0e9/104.0e6, CLKFBOUT_MULT_F = 10.0, CLKOUT0_DIVIDE_F = 10.0)
  val clk_bufd = BUFG.on(mmce.CLKOUT0)
  val rstSynced = ResetBridge.on(Array(!mmce.LOCKED, io.reset), clk_bufd)
  mmce.CLKIN1 := io.ifclk // no buffer since ifclk PIN is an MRCC I/O
  mmce.CLKFBIN := BUFG.on(mmce.CLKFBOUT)
  mmce.RST := False
  mmce.PWRDWN := False

  val ifclk_domain =
    ClockDomain(
      clock = clk_bufd,
      reset = rstSynced,
      config = ClockDomainConfig(resetKind = SYNC)
    )

  new ClockingArea(ifclk_domain) {
    val gpio = BufferCC(io.gpio)
    val top = HsiLoopbackTest()

    top.io.mode := gpio
    top.io.fx3.dq.read := io.dq
    when(top.io.fx3.dq.writeEnable) { io.dq := top.io.fx3.dq.write }
    io.slwr_n := top.io.fx3.wr_n
    io.slrd_n := top.io.fx3.rd_n
    io.sloe_n := top.io.fx3.oe_n
    io.pktend_n := top.io.fx3.pktend_n
    top.io.fx3.empty_n := io.flaga
    top.io.fx3.full_n := io.flagb
    io.led1_n := !top.io.activity
  }
}
 
object Board_2_14 {
  def main(args: Array[String]) {
    val report = SpinalConfig(defaultConfigForClockDomains =
      ClockDomainConfig(resetActiveLevel = HIGH),
      device=Device.XILINX
    ).generateVerilog(Board_2_14())
  }
}
