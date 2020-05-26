package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7._
import andreasWallner.blackbox.xilinx._
import andreasWallner.test.HsiLoopbackTest

case class DebugBoard() extends Component {
  val io = new Bundle {
    val led1 = in Bits(10 bit)
    val led2 = in Bits(10 bit)
    val led3 = in Bits(10 bit)
    val switch = out Bits(4 bit)

    val A_6_10 = out Bits(5 bit)
    val B_6_10 = out Bits(5 bit)
    val A_11_12 = in Bits(2 bit)
    val B_11_12 = in Bits(2 bit)
    val C_3_12 = out Bits(10 bit)
    val D_3_12 = out Bits(10 bit)
  }
  new Area {
    io.A_6_10(0) := io.led1(0)
    io.B_6_10(0) := io.led1(1)
    io.A_6_10(1) := io.led1(2)
    io.B_6_10(1) := io.led1(3)
    io.A_6_10(2) := io.led1(4)
    io.B_6_10(2) := io.led1(5)
    io.A_6_10(3) := io.led1(6)
    io.B_6_10(3) := io.led1(7)
    io.A_6_10(4) := io.led1(8)
    io.B_6_10(4) := io.led1(9)

    io.C_3_12(0) := io.led2(0)
    io.D_3_12(0) := io.led2(1)
    io.C_3_12(1) := io.led2(2)
    io.D_3_12(1) := io.led2(3)
    io.C_3_12(2) := io.led2(4)
    io.D_3_12(2) := io.led2(5)
    io.C_3_12(3) := io.led2(6)
    io.D_3_12(3) := io.led2(7)
    io.C_3_12(4) := io.led2(8)
    io.D_3_12(4) := io.led2(9)
    
    io.C_3_12(5) := io.led3(0)
    io.D_3_12(5) := io.led3(1)
    io.C_3_12(6) := io.led3(2)
    io.D_3_12(6) := io.led3(3)
    io.C_3_12(7) := io.led3(4)
    io.D_3_12(7) := io.led3(5)
    io.C_3_12(8) := io.led3(6)
    io.D_3_12(8) := io.led3(7)
    io.C_3_12(9) := io.led3(8)
    io.D_3_12(9) := io.led3(9)

    io.switch(0) := io.A_11_12(0)
    io.switch(1) := io.B_11_12(0)
    io.switch(2) := io.A_11_12(1)
    io.switch(3) := io.B_11_12(1)
  }
}

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

    val A_3_14 = inout(Analog(Bits(12 bit)))
    val B_3_14 = inout(Analog(Bits(12 bit)))
    val A_18_30 = inout(Analog(Bits(13 bit)))
    val B_18_30 = inout(Analog(Bits(13 bit)))
    val C_3_15 = inout(Analog(Bits(13 bit)))
    val D_3_15 = inout(Analog(Bits(13 bit)))
    val C_19_30 = inout(Analog(Bits(12 bit)))
    val D_19_30 = inout(Analog(Bits(12 bit)))
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
      config = ClockDomainConfig(resetKind = SYNC),
      frequency = FixedFrequency(100 MHz)
    )

  new ClockingArea(ifclk_domain) {
    val debug = DebugBoard()
    val gpio = BufferCC(io.gpio)
    val top = HsiLoopbackTest()

    io.A_3_14(3 to 7) := debug.io.A_6_10
    io.B_3_14(3 to 7) := debug.io.B_6_10
    io.C_3_15(0 to 9) := debug.io.C_3_12
    io.D_3_15(0 to 9) := debug.io.D_3_12
    debug.io.A_11_12 := io.A_3_14(8 to 9)
    debug.io.B_11_12 := io.B_3_14(8 to 9)

    debug.io.led1(3 to 9).clearAll()
    debug.io.led2.clearAll()
    debug.io.led3.clearAll()
    debug.io.led1(0 to 3) := gpio

    top.io.mode := gpio
    top.io.fx3.dq.read := io.dq
    when(top.io.fx3.dq.writeEnable) { io.dq := top.io.fx3.dq.write }
    io.slwr_n := top.io.fx3.wr_n
    io.slrd_n := top.io.fx3.rd_n
    io.sloe_n := top.io.fx3.oe_n
    io.pktend_n := top.io.fx3.pktend_n
    top.io.fx3.empty_n := io.flaga
    top.io.fx3.full_n := io.flagb
    io.led1_n := !top.io.toggle1Hz
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
