package andreasWallner.intro

// WIP Steps for introducing SpinalHDL

import andreasWallner.eda.YosysFlow
import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib.eda.bench.Rtl

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
  val bitCnt = Reg(UInt(log2Up(3*8) bit))

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
    bitCnt := 3*8-1

    dout := True
  }
  when(busy) {
    when(io.dout && ((shiftReg(0) && longCntReached) || (!shiftReg(0) && shortCntReached))) {
      counter := 0
      dout := False
    } elsewhen(!io.dout && ((shiftReg(0) && shortCntReached) || (!shiftReg(0) && longCntReached))) {
      shiftReg := True ## shiftReg.dropLow(1)
      counter := 0
      bitCnt := bitCnt - 1
      when(bitCnt =/= 0) {
        dout := True
      } otherwise {
        busy := False
      }
    } otherwise {
      dout := io.dout
    }
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

case class IceStickSimpleWG2812() extends Component {
  val io = new Bundle {
    val pmod = out port Bits(8 bit)
  }
  val delay = ((100 ms) * (12 MHz)).toBigInt()
  val counter = Reg(UInt(log2Up(delay + 1) bit)) init 0

  io.pmod(7 downto 1).clearAll()
  val wg2812 = SimpleWG2812()
  val r = Reg(wg2812.io.r.clone()) init 0
  io.pmod(0) := wg2812.io.dout
  wg2812.io.r := r
  wg2812.io.g := 0x80
  wg2812.io.b := 0x20

  wg2812.io.valid := counter === 0
  when(counter =/= 0) {
    counter := counter - 1
  } elsewhen(wg2812.io.valid && wg2812.io.ready) {
    r := r + 0x10
    counter := delay
  }
}

object IceStickSimpleWG2812Test extends App {
  val dut = SpinalSimConfig().withFstWave.compile(IceStickSimpleWG2812())
  dut.doSim("smoketest") { dut =>
    dut.clockDomain.forkStimulus(10)
    sleep(10000)
  }
}

object IceStickSimpleWG2812 extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog { IceStickSimpleWG2812() }
  val synth = YosysFlow(
    "icestick_demo",
    Rtl(report),
    "ice40",
    "hx1k",
    "tq144",
    frequencyTarget = Some(12 MHz),
    pcfFile = Some("icestick.pcf"),
    yosysPath = "/opt/oss-cad-suite-20230105/bin/",
    nextpnrPath = "/opt/oss-cad-suite-20230105/bin/",
    icestormPath = "/opt/oss-cad-suite-20230105/bin/",
    verbose = true
  )
  println(synth.getFMax())
  println(synth.getArea())

  import scala.sys.process._
  println(s"iceprog ${synth.bitstreamFile}" !!)
}

object SimpleWG2812ExtTest extends App {
  val dut = SpinalSimConfig().withFstWave.compile(SimpleWG2812())
  dut.doSim("smoketest") { dut =>
    SimTimeout(50000)

    var rxd = List[Int]()
    WG2812SimRx(dut.io.dout, 100, 60)(i => rxd = rxd :+ i)

    dut.io.r #= 0x55
    dut.io.g #= 0xf0
    dut.io.b #= 0x78
    dut.io.valid #= true

    dut.clockDomain.forkStimulus(10)

    dut.clockDomain.waitSamplingWhere(dut.io.ready.toBoolean)
    dut.io.valid #= false
    dut.clockDomain.waitSampling()

    waitUntil(dut.io.idle.toBoolean)

    assert(rxd == List(0xF0, 0x55, 0x78))

    sleep(100)
  }
}

