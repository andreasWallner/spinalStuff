package andreasWallner.intro


import andreasWallner.ext.PimpedPhysicalNumber
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

case class RGB(width: BitCount) extends Bundle {
  val r = UInt(width)
  val g = UInt(width)
  val b = UInt(width)

  def init(colors: (Int, Int, Int)) = {
    r init colors._1
    g init colors._2
    b init colors._3
    this
  }
}

case class SimpleStreamWG2812() extends Component {
  val io = new Bundle {
    val colors = slave(Stream(RGB(8 bit)))
    val idle = out port Bool()
    val dout = out port Bool().setAsReg() init False
  }

  val freq = ClockDomain.current.frequency.getValue
  val rstCnt = ((50 us) * freq).toBigInt
  val shortCnt = ((.425 us) * freq).toBigInt
  val longCnt = ((.825 us) * freq).toBigInt

  val shortTime = freq.toTime * BigDecimal(shortCnt)
  val longTime = freq.toTime * BigDecimal(longCnt)
  assert(shortTime > (0.3 us), f"short time not within spec (${shortTime.decomposeString} < 0.3us)")
  assert(shortTime < (0.55 us), f"short time not within spec (${shortTime.decomposeString} > 0.55us)")
  assert(longTime > (0.7 us), f"long time not within spec (${longTime.decomposeString} < 0.7us)")
  assert(longTime < (1.0 us), f"long time not within spec (${longTime.decomposeString} > 1.0us)")

  val counter = Reg(UInt(log2Up(rstCnt + 1) bit)) init 0
  val shortCntReached = counter === shortCnt
  val longCntReached = counter === longCnt
  val rstCntReached = counter === rstCnt
  val shiftReg = Reg(Bits(3 * 8 bit))
  val bitCnt = Reg(UInt(log2Up(3*8) bit))

  io.colors.ready := False
  val busy = Reg(Bool()) init False
  when(!rstCntReached) {
    counter := counter + 1
  }
  when(!busy && io.colors.valid && rstCntReached) {
    shiftReg := io.colors.b.reversed ## io.colors.r.reversed ## io.colors.g.reversed
    io.colors.ready := True
    busy := True
    counter := 0
    bitCnt := 3*8-1

    io.dout := True
  }
  val next = (io.dout === shiftReg(0)) ? longCntReached | shortCntReached
  when(busy) {
    when(io.dout && next) {
      counter := 0
      io.dout := False
    } elsewhen(!io.dout && next) {
      shiftReg := True ## shiftReg.dropLow(1)
      counter := 0
      bitCnt := bitCnt - 1
      when(bitCnt =/= 0) {
        io.dout := True
      } otherwise {
        busy := False
      }
    } otherwise {
      io.dout := io.dout
    }
    assert(counter =/= rstCnt, "counter should never reach reset count when not done")
  }
  io.idle := !busy
}

object SimpleStreamWG2812Test extends App {
  val dut = SpinalSimConfig().withConfig(
    SpinalConfig(defaultClockDomainFrequency = FixedFrequency(12 MHz))
  ).withFstWave.compile(SimpleStreamWG2812())
  dut.doSim("smoketest") { dut =>
    SimTimeout(50000)

    var rxd = List[Int]()
    WG2812SimRx(dut.io.dout, 100, 60){ i => rxd = rxd :+ i }

    dut.io.colors.r #= 0x55
    dut.io.colors.g #= 0xf0
    dut.io.colors.b #= 0x78
    dut.io.colors.valid #= true

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSamplingWhere(dut.io.colors.ready.toBoolean)
    dut.io.colors.valid #= false
    dut.clockDomain.waitSampling()

    waitUntil(dut.io.idle.toBoolean)

    assert(rxd == List(0xF0, 0x55, 0x78))

    sleep(1000)
  }
}
