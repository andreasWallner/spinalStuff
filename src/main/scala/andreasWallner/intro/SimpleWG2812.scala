package andreasWallner.intro

import spinal.core._
import spinal.core.sim.SpinalSimConfig

import scala.language.postfixOps

case class SimpleWG2812() extends Component {
  val io = new Bundle {
    val r = in port UInt(8 bit)
    val g = in port UInt(8 bit)
    val b = in port UInt(8 bit)
    val valid = in port Bool()
    val ready = out port Bool()

    val idle = out port Bool()

    val dout = out port Bool()
  }

  val dout = Bool()
  io.dout := RegNext(dout) init False

  val frequency = 12 MHz
  val rstCnt = ((50 us) * frequency).toBigInt()
  val shortCnt = ((.425 us) * frequency).toBigInt()
  val longCnt = ((.825 us) * frequency).toBigInt()

  val counter = Reg(UInt(10 bit)) init 0
  val shortCntReached = counter === shortCnt
  val longCntReached = counter === longCnt
  val rstCntReached = counter === rstCnt
  val shiftReg = Reg(Bits(3 * 8 bit))
  val bitCnt = Reg(UInt(log2Up(15 + 1) bit))

  dout := False
  io.ready := False
  val busy = Reg(Bool()) init False
  when(!rstCntReached) {
    counter := counter + 1
  }
  when(!busy && io.valid && rstCntReached) {
    shiftReg := io.b.reversed ## io.r.reversed ## io.g.reversed
    io.ready := True
    busy := True
    counter := 0
    bitCnt := 15

    dout := True
  }
  when(busy) {
    when(io.dout && ((shiftReg(0) && longCntReached) || (!shiftReg(0) && shortCntReached))) {
      counter := 0
      dout := False
      bitCnt := bitCnt - 1
    } elsewhen(!io.dout && ((shiftReg(0) && shortCntReached) || (!shiftReg(0) && longCntReached))) {
      shiftReg := True ## shiftReg.dropLow(1)
      counter := 0
      when(bitCnt =/= 0) {
        dout := True
      } otherwise {
        busy := False
      }
    } otherwise {
      dout := io.dout
    }
    assert(counter =/= rstCnt, "counter should never reach reset count when not done")
  }
  io.idle := !busy
}

object SimpleWG2812 extends App {
  SpinalVerilog(SimpleWG2812())
}

import spinal.core.sim._

object SimpleWG2812Test extends App {
  val dut = SpinalSimConfig().withFstWave.compile(SimpleWG2812())
  dut.doSim("smoketest") { dut =>
    SimTimeout(50000)
    dut.io.r #= 0x55
    dut.io.g #= 0xf0
    dut.io.b #= 0x78
    dut.io.valid #= true

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.valid #= false
    dut.clockDomain.waitSampling()

    waitUntil(dut.io.idle.toBoolean)

    sleep(100)
  }
}

case class WG2812Sim(d: Bool, highTime: Int, lowTime: Int)(cb: Int => Unit) {
  val threshold = (highTime + lowTime) / 2
  fork {
    while(true) {
      waitUntil(d.toBoolean)
      val riseTime = simTime()
      waitUntil(!d.toBoolean)
      val fallTime = simTime()
      bit(fallTime - riseTime < threshold)
    }
  }

  var bits = List[Boolean]()
  def bit(b: Boolean): Unit = {
    println(f"${simTime()} rx bit $b")
    bits = bits :+ b
    if(bits.length == 8)
      byte(bits)
  }

  def byte(bs: List[Boolean]): Unit = {
    val i = bs.foldLeft(0)((ii, b) => (ii << 1) + b.toInt)
    cb(i)
  }
}
object SimpleWG2812ExtTest extends App {
  val dut = SpinalSimConfig().withFstWave.compile(SimpleWG2812())
  dut.doSim("smoketest") { dut =>
    SimTimeout(50000)

    WG2812Sim(dut.io.dout, 100, 60)(i => println(f"${simTime()} rxd $i%02x"))

    dut.io.r #= 0x55
    dut.io.g #= 0xf0
    dut.io.b #= 0x78
    dut.io.valid #= true

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.valid #= false
    dut.clockDomain.waitSampling()

    waitUntil(dut.io.idle.toBoolean)

    sleep(100)
  }
}
