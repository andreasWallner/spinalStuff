package andreasWallner.io.iso7816

import andreasWallner.registers.BusSlaveFactoryRecorder
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.io._
import spinal.lib.fsm._

class StateDelay(cyclesCount: UInt)(implicit stateMachineAccessor: StateMachineAccessor) extends State with StateCompletionTrait {

  /** Create a StateDelay with an TimeNumber */
  def this(time: TimeNumber)(implicit stateMachineAccessor: StateMachineAccessor){
    this((time * ClockDomain.current.frequency.getValue).toBigInt)
  }

  val cache = stateMachineAccessor.cacheGetOrElseUpdate(StateMachineSharableUIntKey, new StateMachineSharableRegUInt).asInstanceOf[StateMachineSharableRegUInt]
  cache.addMinWidth(cyclesCount.getWidth)

  onEntry{
    cache.value := cyclesCount.resized
  }

  whenIsActiveWithPriority(1){
    cache.value := cache.value - 1
    when(cache.value <= 1){
      doWhenCompletedTasks()
    }
  }
}

case class S10() extends Bundle with IMasterSlave {
  val clk = Bool()
  val rst = Bool()
  val data = TriState(Bool())

  override def asMaster(): Unit = {
    out(clk, rst)
    master(data)
  }
  override def asSlave(): Unit = {
    in(clk, rst)
    master(data)
  }

  override def setAsReg(): this.type = {
    clk.setAsReg()
    rst.setAsReg()
    data.write.setAsReg()
    data.writeEnable.setAsReg()
    this
  }
}

object Action extends SpinalEnum(binarySequential) {
  val vcc_on, reset, clock, write, dummyWrite, transmit0, transmit1, receive, vcc_off = newElement()
}

case class S10Master(longCntWidth: Int, cntWidth: Int) extends Component {
  val io = new Bundle {
    val iso = master port S10()
    val vcc = out port Bool().setAsReg()

    val cmd = slave port Stream(Action())
    val rsp = master port Flow(Bool())
    val busy = out port Bool()

    val timings = new Bundle {
      val por = in port UInt(longCntWidth bit)
      val low_clk = in port UInt(cntWidth bit)
      val post_write_low_clk = in port UInt(cntWidth bit)
      val high_clk_en = in port UInt(cntWidth bit)
      val high_clk_rem = in port UInt(cntWidth bit)

      val rst_pre_post = in port UInt(cntWidth bit)
      val rst = in port UInt(longCntWidth bit)

      val write_pulse_pre_post = in port UInt(cntWidth bit)
      val write_pulse = in port UInt(cntWidth bit)
      val write = in port UInt(longCntWidth bit)
      val write_cnt = in port UInt(8 bit)
      val dummy = in port UInt(cntWidth bit)
    }
  }
  io.iso.setAsReg()
  io.vcc init False
  io.iso.clk init False
  io.iso.rst init False
  io.iso.data.writeEnable init False
  io.iso.data.write init False

  io.cmd.ready := False
  io.rsp.payload := BufferCC(io.iso.data.read)
  io.rsp.valid := False
  io.busy := True

  val lastWasWrite = Reg(Bool()) init False
  val clkWidth = lastWasWrite.mux(f = io.timings.low_clk, t = io.timings.post_write_low_clk)

  //noinspection ForwardReference
  val fsm = new StateMachine {
    val idle = new State with EntryPoint {
      whenIsActive {
        io.busy := io.cmd.valid
        when(io.cmd.valid) {
          switch(io.cmd.payload) {
            is(Action.vcc_on) { goto(vcc_on) }
            is(Action.reset) { goto(clk_low) }
            is(Action.clock) { goto(clk_low) }
            is(Action.write) { goto(pulse_pre) }
            is(Action.dummyWrite) { goto(pulse_pre) }
            is(Action.transmit0, Action.transmit1) { goto(clk_low) }
            is(Action.receive) { goto(clk_low) }
            is(Action.vcc_off) { goto(vcc_off) }
            //default { io.cmd.ready := True }
          }
        }
      }
    }

    val vcc_on: State = new StateDelay(io.timings.por) {
      whenIsActive {
        io.vcc := True
      }
      whenCompleted {
        io.cmd.ready := True
        goto(idle)
      }
    }

    val vcc_off: State = new State {
      whenIsActive {
        lastWasWrite := False
        io.iso.data.writeEnable := False
        io.vcc := False
        io.cmd.ready := True
        goto(idle)
      }
    }

    def fallback(): Unit = {
      default {
        io.cmd.ready := True
        goto(idle)
      }
    }

    val clk_low: State = new StateDelay(clkWidth) {
      whenIsActive {
        io.iso.clk := False
      }
      whenCompleted {
        lastWasWrite := False
        switch(io.cmd.payload) {
          is(Action.reset) { goto(rst_high_pre) }
          is(Action.clock) { goto(clk_high_1) }
          is(Action.receive) { goto(clk_high_1) }
          is(Action.transmit0, Action.transmit1) { goto(clk_high_1) }
          fallback()
        }
      }
    }

    val clk_high_1: State = new StateDelay(io.timings.high_clk_en) {
      whenIsActive {
        io.iso.clk := True
        io.iso.data.write := io.cmd.payload === Action.transmit1
      }
      whenCompleted {
        io.iso.data.writeEnable := io.cmd.payload === Action.transmit0 || io.cmd.payload === Action.transmit1
        goto(clk_high_2)
      }
    }

    val clk_high_2: State = new StateDelay(io.timings.high_clk_rem) {
      whenCompleted {
        io.iso.clk := False
        switch(io.cmd.payload) {
          is(Action.reset) { goto(rst_high_post) }
          is(Action.clock) { io.cmd.ready := True; goto(idle) }
          is(Action.receive) { io.cmd.ready := True; io.rsp.valid := True; goto(idle) }
          is(Action.transmit0, Action.transmit1) { io.cmd.ready := True; goto(idle) }
          fallback()
        }
      }
    }

    // chip reset pulse

    val rst_high_pre: State = new StateDelay(io.timings.rst_pre_post) {
      whenIsActive {
        io.iso.rst := True
        io.iso.data.writeEnable := False
      }
      whenCompleted { goto(rst) }
    }

    val rst: State = new StateDelay(io.timings.rst) {
      whenIsActive { io.iso.clk := True }
      whenCompleted { goto(rst_high_post) }
    }

    val rst_high_post: State = new StateDelay(io.timings.rst_pre_post) {
      whenIsActive { io.iso.clk := False }
      whenCompleted {
        io.iso.rst := False
        io.cmd.ready := True
        goto(idle)
      }
    }

    // write/dummy write pulses

    val pulse_pre: State = new StateDelay(io.timings.write_pulse_pre_post) {
      whenCompleted {
        goto(pulse_high)
        io.iso.data.writeEnable := False
      }
    }

    val pulse_high: State = new StateDelay(io.timings.write_pulse) {
      whenIsActive { io.iso.rst := True }
      whenCompleted {
        io.iso.rst := False
        goto(pulse_post)
      }
    }

    val write_cnt = Reg(UInt(8 bit))
    val pulse_post: State = new StateDelay(io.timings.write_pulse_pre_post) {
      whenCompleted {
        io.iso.rst := False
        write_cnt := 0
        switch(io.cmd.payload) {
          is(Action.write) { goto(write) }
          is(Action.dummyWrite) { goto(dummyWrite) }
          fallback()
        }
      }
    }

    val write: State = new StateDelay(io.timings.write) {
      whenIsActive {
        io.iso.clk := True
        lastWasWrite := True
      }
      whenCompleted {
        goto(write_decide)
      }
    }

    val write_decide: State = new State {
      whenIsActive {
        when(write_cnt =/= io.timings.write_cnt) {
          write_cnt := write_cnt + 1
          goto(write)
        } otherwise {
          io.iso.clk := False
          io.cmd.ready := True
          goto(idle)
        }
      }
    }

    val dummyWrite: State = new StateDelay(io.timings.dummy) {
      whenIsActive {
        io.iso.clk := True
      }
      whenCompleted {
        io.iso.clk := False
        io.cmd.ready := True
        goto(idle)
      }
    }
  }
}

import spinal.lib.bus.misc.BusSlaveFactory
case class S10MasterPeripheral[T <: Data with IMasterSlave](
    prescaler: Int,
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component {
  val io = new Bundle {
    val iso = master port S10()
    val bus = slave(busType())
    val vcc = out port Bool()
    val busy = out port Bool()
  }

  val by1024 = new SlowArea(prescaler) {
    val core = S10Master(11, 8)
    core.io.iso <> io.iso
    core.io.busy <> io.busy
    io.vcc := core.io.vcc
  }
  val cmdFifo = StreamFifo(Bits(4 bit), 1024)
  val rspFifo = StreamFifo(Bits(8 bit), 127)

  val en = Reg(Bool()) init False
  by1024.core.io.cmd.payload.assignFromBits(cmdFifo.io.pop.payload)
  by1024.core.io.cmd.valid := cmdFifo.io.pop.valid & en
  cmdFifo.io.pop.ready := by1024.core.io.cmd.ready & by1024.clockDomain.readClockEnableWire

  val factory = new BusSlaveFactoryRecorder(metaFactory(io.bus))

  val storeRsp = False
  cmdFifo.io.flush := False
  rspFifo.io.flush := False
  val trigger = factory.register("trigger")
  trigger.setOnSet(cmdFifo.io.flush, 0, "flush_cmd")
  trigger.setOnSet(rspFifo.io.flush, 1, "flush_rsp")
  trigger.setOnSet(en, 2, "cmds")
  en.clearWhen(!cmdFifo.io.pop.valid)
  trigger.setOnSet(storeRsp, 3, "store_rsp")
  val status = factory.register("status0")
  status.read(by1024.core.io.busy, 0, "busy")
  status.read(rspFifo.io.occupancy, 1, "rsp_occ")

  val tim = by1024.core.io.timings
  tim.por(7 downto 0) := factory.register("tim0").createReadAndWrite(UInt(8 bit), 0, "por_lo")
  tim.por(10 downto 8) := factory.register("tim1").createReadAndWrite(UInt(3 bit), 0, "pow_hi")

  tim.low_clk := factory.register("tim2").createReadAndWrite(UInt(8 bit), 0, "low_clk")
  tim.post_write_low_clk := factory.register("tim3").createReadAndWrite(UInt(8 bit), 0, "post_write_low_clk")
  tim.high_clk_en := factory.register("tim4").createReadAndWrite(UInt(8 bit), 0, "high_clk_en")
  tim.high_clk_rem := factory.register("tim5").createReadAndWrite(UInt(8 bit), 0, "high_clk_rem")
  tim.rst_pre_post := factory.register("tim6").createReadAndWrite(UInt(8 bit), 0, "rst_pre_post")
  tim.rst(7 downto 0) := factory.register("tim7").createReadAndWrite(UInt(8 bit), 0, "rst_lo")
  tim.rst(10 downto 8) := factory.register("tim8").createReadAndWrite(UInt(3 bit), 0, "rst_hi")

  tim.write_pulse_pre_post := factory.register("tim9").createReadAndWrite(UInt(8 bit), 0, "write_pulse_pre_post")
  tim.write_pulse := factory.register("tim10").createReadAndWrite(UInt(8 bit), 0, "write_pulse")
  tim.write(7 downto 0) := factory.register("tim11").createReadAndWrite(UInt(8 bit), 0, "write_lo")
  tim.write(10 downto 8) := factory.register("tim12").createReadAndWrite(UInt(3 bit), 0, "write_hi")
  tim.write_cnt := factory.register("tim13").createReadAndWrite(UInt(8 bit), 0, "write_cnt")
  tim.dummy := factory.register("tim14").createReadAndWrite(UInt(8 bit), 0, "dummy_write")

  //val vcc_on, reset, clock, write, dummyWrite, transmit, receive, vcc_off = newElement()
  factory.register("cmd").createAndDriveFlow(Bits(4 bit), 0, "action").toStream >> cmdFifo.io.push

  rspFifo.io.pop.ready := False
  factory.register("resp").read(rspFifo.io.pop.payload, 0, "bits")
  factory.factory.onRead(18) { rspFifo.io.pop.ready := True }

  val rspBuffer = Reg(Bits(8 bit)) init B"10000000"
  val nextBuffer = by1024.core.io.rsp.payload ## rspBuffer(7 downto 1)
  rspFifo.io.push.valid := False
  rspFifo.io.push.payload := nextBuffer
  when(rspFifo.io.flush) {
    rspBuffer := B"10000000"
  } elsewhen ((by1024.core.io.rsp.valid && by1024.clockDomain.readClockEnableWire) || storeRsp) {
    rspBuffer := nextBuffer
    when(storeRsp) {
      rspFifo.io.push.valid := True
      rspFifo.io.push.payload := rspBuffer
      rspBuffer := B"10000000"
    } elsewhen(rspBuffer(0)) {
      rspFifo.io.push.valid := True
      rspBuffer := B"10000000"
    }
  }
}
