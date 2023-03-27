package andreasWallner.intro

import andreasWallner.sim.simLog
import andreasWallner.eda.YosysFlow
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import spinal.lib.eda.bench.Rtl
import spinal.lib.sim._

import scala.language.postfixOps

case class FragmentWG2812() extends Component {
  val io = new Bundle {
    val colors = slave port Stream(Fragment(UInt(8 bit)))
    val idle = out port Bool()
    val dout = out port Bool() setAsReg () init False
  }

  val (rstCnt, shortCnt, longCnt) = WG2812.calculateTimings()

  val wasLast = RegNextWhen(io.colors.last, io.colors.fire)
  io.colors.setBlocked()
  io.idle := True

  val fsm = new StateMachine {
    val waitRst: State = new StateDelay(rstCnt) with EntryPoint {
      whenIsActive { io.idle := False }
      whenCompleted(goto(idle))
    }

    val buffer = Reg(Bits(9 bit)) init B"000000010"
    val done = buffer === M"00000001-"
    val idle: State = new State {
      whenIsActive {
        when(io.colors.valid) {
          io.colors.ready := True
          buffer := True ## io.colors.fragment.reversed
          goto(driveHigh)
        }
      }
    }

    val timingCnt = Reg(UInt(log2Up(longCnt) bit))
    val shortCntReached = timingCnt === shortCnt
    val longCntReached = timingCnt === longCnt
    val highDone = buffer(0).mux(False -> shortCntReached, True -> longCntReached)
    val driveHigh: State = new State {
      onEntry {
        timingCnt := 0
        io.dout := True
      }
      whenIsActive {
        io.idle := False
        timingCnt := timingCnt + 1
        when(highDone) { goto(driveLow) }
      }
    }

    val lowDone = buffer(0).mux(False -> longCntReached, True -> shortCntReached)
    val driveLow: State = new State {
      onEntry {
        timingCnt := 0
        io.dout := False // has this as True
      }
      whenIsActive {
        timingCnt := timingCnt + 1
        io.idle := False
        when(lowDone) {
          when(!done) {
            buffer := buffer |>> 1 // had that in onExit
            goto(driveHigh)
          } elsewhen (wasLast) {
            goto(waitRst)
          } otherwise {
            io.colors.ready := True
            buffer := True ## io.colors.fragment.reversed
            goto(driveHigh)
          }
        }
      }
    }
  }
}

object FragmentWG2812 extends App {
  val dut = SpinalSimConfig()
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(12 MHz))
    )
    .withFstWave
    .compile(FragmentWG2812())
  dut.doSim("smoketest") { dut =>
    SimTimeout(50000)

    val scoreboard = ScoreboardInOrder[Int]()
    var remaining = 9
    StreamDriver(dut.io.colors, dut.clockDomain) { payload =>
      remaining = remaining - 1
      payload.fragment.randomize()
      payload.last #= remaining == 0
      remaining >= 0
    }.transactionDelay = () => 0
    StreamMonitor(dut.io.colors, dut.clockDomain) { payload =>
      //simLog(f"REF ${payload.fragment.toInt}")
      scoreboard.pushRef(payload.fragment.toInt)
    }
    WG2812SimRx(dut.io.dout, 100, 60) { i =>
      //simLog(f"DUT ${i}")
      scoreboard.pushDut(i)
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSamplingWhere(dut.io.colors.ready.toBoolean)
    dut.clockDomain.waitSamplingWhere(dut.io.idle.toBoolean)
    sleep(1000)

    print(scoreboard.matches)
    assert(scoreboard.matches == 9)
    scoreboard.checkEmptyness()
  }
}

case class IceStickFragmentWG2812(
    leds: Int = 7,
    skipDelay: TimeNumber = 100 ms,
    colorChange: TimeNumber = 10 ms
) extends Component {
  val io = new Bundle {
    val pmod = out port Bits(8 bit)
  }
  io.pmod(7 downto 1).clearAll()

  val wg2812 = FragmentWG2812()
  wg2812.io.colors.valid := False
  wg2812.io.colors.last := False
  wg2812.io.colors.fragment := 0
  io.pmod(0) := wg2812.io.dout

  val skipTiming = new Area {
    val cycles = (skipDelay * ClockDomain.current.frequency.getValue).toBigInt()
    val cnt = Reg(UInt(log2Up(cycles) bit)) init 0
    val fire = cnt === 0
    when(!fire) {
      cnt := cnt - 1
    }

    def restart(): Unit = {
      cnt := cycles - 1
    }
  }

  val colorGen = new Area {
    val color = Reg(RGB(8 bit)) init (0, 0, 100)
    color.r.allowUnsetRegToAvoidLatch()
    val cycles = (colorChange * ClockDomain.current.frequency.getValue).toBigInt()
    val colorCnt = Reg(UInt(log2Up(cycles) bit)) init (cycles - 1)

    val moreG = Reg(Bool()) init True
    val cntUp = (moreG || color.g === 0) & !(color.b === 0)
    when(colorCnt === 0) {
      when(cntUp) {
        moreG := True
        color.g := color.g + 10
        color.b := color.b - 10
      } otherwise {
        moreG := False
        color.g := color.g - 10
        color.b := color.b + 10
      }
      colorCnt := cycles - 1
    } otherwise {
      colorCnt := colorCnt - 1
    }
  }

  val fsm = new StateMachine {
    val activeLed = Reg(UInt(log2Up(leds) bit))
    val sampledColor = Reg(RGB(8 bit))
    val ledCnt = Reg(UInt(log2Up(leds) bit))
    val idle: State = new State with EntryPoint {
      whenIsActive {
        ledCnt := 0
        when(skipTiming.fire) {
          skipTiming.restart()
          sampledColor := colorGen.color
          goto(transmitG)
        }
      }
    }

    val transmitG: State = new State {
      whenIsActive {
        wg2812.io.colors.fragment := (activeLed === ledCnt) ? sampledColor.g | 0
        wg2812.io.colors.valid := True
        when(wg2812.io.colors.fire) { goto(transmitR) }
      }
    }

    val transmitR: State = new State {
      whenIsActive {
        wg2812.io.colors.fragment := (activeLed === ledCnt) ? sampledColor.r | 0
        wg2812.io.colors.valid := True
        when(wg2812.io.colors.fire) { goto(transmitB) }
      }
    }

    val lastLed = ledCnt === (leds - 1)
    val transmitB: State = new State {
      whenIsActive {
        wg2812.io.colors.fragment := (activeLed === ledCnt) ? sampledColor.b | 0
        wg2812.io.colors.valid := True
        wg2812.io.colors.last := lastLed
        when(wg2812.io.colors.fire) {
          ledCnt := ledCnt + 1
          when(lastLed) {
            goto(idle)
            when(activeLed === (leds - 2)) {
              activeLed := 0
            } otherwise {
              activeLed := activeLed + 1
            }
          } otherwise {
            goto(transmitG)
          }
        }
      }
    }
  }
}

object IceStickFragmentWG2812Test extends App {
  val dut = SpinalSimConfig()
    .withConfig(
      SpinalConfig(defaultClockDomainFrequency = FixedFrequency(12 MHz))
    )
    .withFstWave
    .compile {
      new Component {
        val io = new Bundle {
          val dout = out port Bool()
        }
        io.dout := IceStickFragmentWG2812(skipDelay = 400 us, colorChange = 100 us).io.pmod(0)
      }.setDefinitionName("IceStickFragmentWG2812Tester")
    }
  dut.doSim("smoketest") { dut =>
    WG2812SimRx(dut.io.dout, 100, 60) { i =>
      simLog(f"Got $i%02x")
    }
    dut.clockDomain.forkStimulus(10)

    sleep(1000000)
  }
}

object IceStickFragmentWG2812 extends App {
  val report = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH),
    defaultClockDomainFrequency = FixedFrequency(12 MHz),
    device = Device.LATTICE
  ).generateVerilog {
    IceStickFragmentWG2812(skipDelay = 100 ms, colorChange = 4000 ms)
  }
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
