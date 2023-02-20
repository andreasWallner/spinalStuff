package andreasWallner.io.pwm

import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.casemodel.{Register, Value}
import andreasWallner.registers.datamodel.BusComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

import scala.collection.immutable
import scala.language.postfixOps

/**
 * Most simple delta modulator, overflowing at 2**width
 * Warning: values bigger than 2**width are folded back and may
 * lead to unintended behavior.
 * With value = 0, io.mod is always disabled. With value = 2**width io.mod
 * is always turned on.
 */
case class DeltaModulator(width: Int) extends Component {
  val io = new Bundle {
    val en = in port Bool()
    val value = in port UInt(width + 1 bits)
    val mod = out port Bool()
  }

  val counter = Reg(UInt(width bits))
  val next = counter + io.value
  when(!io.en) {
    counter := 0
    io.mod := False
  } otherwise {
    counter := next.resized
    io.mod := next(next.high)
  }
}

case class DeltaModulators(channels: Int, width: Int) extends Component {
  val io = new Bundle {
    val en = in port Bool()
    val values = in port Vec(UInt(width + 1 bits), channels)
    val mod = out port Vec(Bool(), channels)
  }

  val modulators = List.fill(width)(DeltaModulator(width))
  (modulators, io.values, io.mod).zipped.foreach((m, v, o) => {
    m.io.en := io.en
    m.io.value := v
    o := m.io.mod
  })
}

class DeltaModulatorCtrl[T <: spinal.core.Data with IMasterSlave](
    prescalerWidth: Int,
    channels: Int, width: Int,
    busType: HardType[T],
    metaFactory: T => BusSlaveFactory
) extends Component with BusComponent {
  val io = new Bundle {
    val bus = slave(busType())
    val mod = out Vec(Bool(), channels)
  }
  val prescalerEnable = False
  val slowArea = new ClockEnableArea(prescalerEnable) {
    val mods = DeltaModulators(channels, width)
  }

  val factory = new BusSlaveFactoryRecorder(metaFactory(io.bus))

  val ctrl = factory.register(0x00, "ctrl")
  slowArea.mods.io.en := ctrl.createReadAndWrite(
    Bool(),
    0,
    "run",
    "Enable or disable counter and module output",
    List(
      Value(0, "dis", "Module disabled"),
      Value(1, "en", "Module enabled")
    )
  ) init False

  val config0 = factory.register(0x04, "config0")
  val divider = new Area {
    val set = config0.createReadAndWrite(
      UInt(prescalerWidth bits),
      0,
      "prescaler",
      "Prescaler for the base clock of the module, f_mod = f / (prescaler + 1)"
    )
    val cnt = Reg(UInt(prescalerWidth bit))

    when(cnt === 0) {
      prescalerEnable := True
      cnt := set
    } otherwise {
      cnt := cnt - 1
    }
  }

  slowArea.mods.io.values.zipWithIndex.foreach { case (lvl, idx) =>
    val reg = factory.register(f"level$idx")
    lvl := reg.createReadAndWrite(
      UInt(width + 1 bits),
      0,
      "val"
    )
  }

  override def elements: immutable.Seq[Register] = factory.elements
  override def busComponentName = "DeltaModulator"
  override def dataWidth: Long = factory.dataWidth
  override def wordAddressInc: Long = factory.wordAddressInc
}
