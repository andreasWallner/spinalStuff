package andreasWallner.ztex

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7._
import andreasWallner.blackbox.xilinx._
import andreasWallner.test.{SlaveFifoLoopback, IOControl}

case class InterfaceTestTop() extends Component {
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

    val gpio_n = inout(Analog(Bits(4 bits))) // with pullups

    val led1_n = out Bool () // T11 <-> led

    val leds1 = out Bits (10 bit)
    val leds2 = out Bits (10 bit)
    val leds3 = out Bits (10 bit)
    val switches = in Bits (4 bit)
  }

  val mmcm = MMCME2_BASE(
    CLKIN1_PERIOD = 1.0e9 / 104.0e6,
    CLKFBOUT_MULT_F = 10.0,
    CLKOUT0_DIVIDE_F = 10.0
  )
  val clk_bufd = BUFG.on(mmcm.CLKOUT0)
  val rstSynced = ResetBridge.on(Array(!mmcm.LOCKED, io.reset), clk_bufd)
  mmcm.CLKIN1 := io.ifclk // no buffer since ifclk PIN is an MRCC I/O
  mmcm.CLKFBIN := BUFG.on(mmcm.CLKFBOUT)
  mmcm.RST := False
  mmcm.PWRDWN := False

  val ifclk_domain =
    ClockDomain(
      clock = clk_bufd,
      reset = rstSynced,
      config = ClockDomainConfig(resetKind = SYNC),
      frequency = FixedFrequency(100 MHz)
    )

  new ClockingArea(ifclk_domain) {
    val gpio = ~BufferCC(io.gpio_n)
    val switches = BufferCC(io.switches)
    val top = SlaveFifoLoopback()

    io.leds1(0 to 3) := gpio
    io.leds1(4) := top.io.activity
    io.leds1(4 to 9).clearAll()
    io.leds2.clearAll()
    io.leds3.clearAll()

    top.io.mode := gpio
    top.io.fx3.dq.read := io.dq
    when(top.io.fx3.dq.writeEnable) { io.dq := top.io.fx3.dq.write }
    io.slwr_n := top.io.fx3.wr_n
    io.slrd_n := top.io.fx3.rd_n
    io.sloe_n := top.io.fx3.oe_n
    io.pktend_n := top.io.fx3.pktend_n
    top.io.fx3.empty_n := io.flaga
    top.io.fx3.full_n := io.flagb
    io.led1_n := top.io.toggle1Hz
  }
}

object InterfaceTestTop {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(InterfaceTestTop())
  }
}

case class IOControlTop() extends Component {
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

    val gpio_n = inout(Analog(Bits(4 bits))) // with pullups

    val led1_n = out Bool () // T11 <-> led

    val leds1 = out Bits (10 bit)
    val leds2 = out Bits (10 bit)
    val leds3 = out Bits (10 bit)
    val switches = in Bits (4 bit)
  }

  val mmcm = MMCME2_BASE(
    CLKIN1_PERIOD = 1.0e9 / 104.0e6,
    CLKFBOUT_MULT_F = 10.0,
    CLKOUT0_DIVIDE_F = 10.0
  )
  val clk_bufd = BUFG.on(mmcm.CLKOUT0)
  val rstSynced = ResetBridge.on(Array(!mmcm.LOCKED, io.reset), clk_bufd)
  mmcm.CLKIN1 := io.ifclk // no buffer since ifclk PIN is an MRCC I/O
  mmcm.CLKFBIN := BUFG.on(mmcm.CLKFBOUT)
  mmcm.RST := False
  mmcm.PWRDWN := False

  val ifclk_domain =
    ClockDomain(
      clock = clk_bufd,
      reset = rstSynced,
      config = ClockDomainConfig(resetKind = SYNC),
      frequency = FixedFrequency(100 MHz)
    )

  new ClockingArea(ifclk_domain) {
    val gpio = ~BufferCC(io.gpio_n)
    val switches = BufferCC(io.switches)
    val top = IOControl()

    io.leds1 := top.io.led
    io.leds2 := top.io.pwmLed
    io.leds3.clearAll()
    io.led1_n := False

    top.io.fx3.dq.read := io.dq
    when(top.io.fx3.dq.writeEnable) { io.dq := top.io.fx3.dq.write }
    io.slwr_n := top.io.fx3.wr_n
    io.slrd_n := top.io.fx3.rd_n
    io.sloe_n := top.io.fx3.oe_n
    io.pktend_n := top.io.fx3.pktend_n
    top.io.fx3.empty_n := io.flaga
    top.io.fx3.full_n := io.flagb
  }
}

object IOControlTop {
  def main(args: Array[String]) {
    val report = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH),
      device = Device.XILINX
    ).generateVerilog(IOControlTop())
  }
}
