package andreasWallner.io.dac

import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.casemodel.Value
import andreasWallner.registers.datamodel.BusComponent
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.misc.BusSlaveFactory

import scala.language.postfixOps

case class StrobedDacGenerics(
    count: Int = 2,
    dataWidth: Int = 8,
    initialValue: Option[Int] = Some(0xff),
    assertPolarity: Boolean = false,
    writeAssertDelayTime: TimeNumber = 5 ns,
    writeAssertTime: TimeNumber = 20 ns,
    writeDeassertHoldTime: TimeNumber = 0 ns
) {
  def adrWidth = log2Up(count)
}

case class DacRequestData(g: StrobedDacGenerics) extends Bundle {
  val adr = UInt(g.adrWidth bit)
  val data = Bits(g.dataWidth bit)
}
case class DacInterface(g: StrobedDacGenerics) extends Bundle {
  val data = Bits(g.dataWidth bit)
  val adr = UInt(g.adrWidth bit)
  val wr = Bool()
}

// DAC interface for DAC with data, address (channel) and
// and write enable like MAX5102
// https://datasheets.maximintegrated.com/en/ds/MAX5102.pdf
// Data and address to be written needs to be written to the io.write
// stream, which will be fired one DAC has been updated
case class StrobedDacPeripheralCore(g: StrobedDacGenerics) extends Component {
  val io = new Bundle {
    val write = slave port Stream(DacRequestData(g))
    val dac = out port DacInterface(g).setAsReg()
  }

  io.dac.wr init Bool(!g.assertPolarity)
  io.write.ready := False

  // Since all output signals are registered, we get at
  // least one cycle in Idle state between two sets
  // which makes sure that we have at least one cycle with
  // wr being deasserted before the next data is applied
  // to the output
  //noinspection ForwardReference
  val fsm = new StateMachine {
    val Idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.write.valid) {
          io.dac.data := io.write.data
          io.dac.adr := io.write.adr
          goto(Setup)
        }
      }
    }
    val Setup = new StateDelay(g.writeAssertDelayTime) {
      whenCompleted { goto(Asserting) }
    }
    val Asserting = new StateDelay(g.writeAssertTime) {
      whenCompleted { goto(Hold) }
      whenIsActive { io.dac.wr := Bool(g.assertPolarity) }
    }
    val Hold = new StateDelay(g.writeDeassertHoldTime) {
      whenCompleted {
        io.dac.wr := Bool(!g.assertPolarity)
        io.write.ready := True
        goto(Idle)
      }
    }
  }
}

class StrobedDacPeripheral[T <: spinal.core.Data with IMasterSlave](
    g: StrobedDacGenerics = StrobedDacGenerics(),
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component
    with BusComponent {
  val io = new Bundle {
    val bus = slave port busType()
    val dac = out port DacInterface(g)
  }
  val mapper = metaFactory(io.bus)
  val f = new BusSlaveFactoryRecorder(mapper)

  val core = StrobedDacPeripheralCore(g)
  core.io.dac <> io.dac

  val resetDone = if(g.initialValue.nonEmpty) False else True
  val writing = Reg(Bool()) init False
  val statusReg = f.register(0, "status")
  statusReg.read(
    writing || !resetDone,
    bitOffset = 0,
    name = "busy",
    doc = "shows whether the peripheral is currently writing to the DAC",
    values = List(
      Value(0, "idle", "Peripheral is idle, a write may be performed"),
      Value(1, "busy", "Peripheral is busy, a write will be ignored")
    )
  )

  val combAdr = UInt(g.adrWidth bit)
  val combData = Bits(g.dataWidth bits)
  val dacReg = f.register(address = 4, name = "adc")
  dacReg.nonStopWrite(
    combAdr,
    bitOffset = 32 - g.adrWidth,
    name = "address",
    doc = "Selector for the DAC channel to write to",
    values = (for (i <- 0 to g.count) yield Value(i, f"channel$i")).toList
  )
  dacReg.nonStopWrite(
    combData,
    bitOffset = 0,
    name = "value",
    doc = "numeric value to write to DAC, no transformation is performed"
  )

  val adr = Reg(UInt(g.adrWidth bit))
  val data = Reg(Bits(g.dataWidth bits))
  core.io.write.adr := adr
  core.io.write.data := data
  core.io.write.valid := writing
  dacReg.onWrite {
    when(!writing) {
      writing := True
      adr := combAdr
      data := combData
    }
  }
  when(core.io.write.fire) { writing := False }

  if(g.initialValue.nonEmpty) {
    val resetFsm = new StateMachine {
      val num = Reg(UInt(g.adrWidth bit)) init 0
      new State with EntryPoint {
        whenIsActive {
          core.io.write.valid := True
          core.io.write.adr := num.resized
          core.io.write.data := g.initialValue.get
          when(core.io.write.fire) {
            when(num === g.count - 1) {
              goto(Done)
            }
            num := num + 1
          }
        }
      }
      val Done = new State {
        whenIsActive {
          resetDone := True
        }
      }
    }
  }

  override def elements = f.elements
  override def busComponentName = "DacCtrl"
  override def dataWidth = f.dataWidth
  override def wordAddressInc = f.wordAddressInc
}
