package andreasWallner.io

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import andreasWallner.registers.BusSlaveFactoryRecorder
import andreasWallner.registers.datamodel.BusComponent
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

// take from spinal.lib.io... with minor modifications
object Gpio {
  case class Parameter(
      width: Int, //Number of pin
      var input: Seq[Int] = null, //List of pin id which can be inputs (null mean all)
      var output: Seq[Int] = null, //List of pin id which can be outputs (null mean all)
      interrupt: Seq[Int] = Nil, //List of pin id which can be used as interrupt source
      readBufferLength: Int = 2 //Number of syncronisation stages
  )

  class Ctrl[T <: spinal.core.Data with IMasterSlave](
      p: Gpio.Parameter,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends Component
      with BusComponent {

    if (p.input == null) p.input = 0 until p.width
    if (p.output == null) p.output = 0 until p.width

    val io = new Bundle {
      val gpio = master(TriStateArray(p.width bits))
      val bus = slave(busType())
      val interrupt = out(Bits(p.width bits))
    }

    val factory = new BusSlaveFactoryRecorder(metaFactory(io.bus))
    val syncronized = Delay(io.gpio.read, p.readBufferLength)
    val last = RegNext(syncronized)

    val read_reg = factory.register(0x00, "read")
    val write_reg = factory.register(0x04, "write")
    val en_reg = factory.register(0x08, "en")
    for (i <- 0 until p.width) {
      if (p.input.contains(i)) read_reg.read(syncronized(i), i, s"I$i")
      if (p.output.contains(i))
        write_reg.driveAndRead(io.gpio.write(i), i, s"O$i")
      else io.gpio.write(i) := False
      if (p.output.contains(i) && p.input.contains(i))
        en_reg.driveAndRead(io.gpio.writeEnable(i), i, s"EN$i") init False
      else io.gpio.writeEnable(i) := Bool(p.output.contains(i))
    }

    val interrupt = new Area {
      val enable = new Area {
        val high, low, rise, fall = Bits(p.width bits)
      }

      val valid = ((enable.high & syncronized)
        | (enable.low & ~syncronized)
        | (enable.rise & (syncronized & ~last))
        | (enable.fall & (~syncronized & last)))

      val rise_flag = factory.register(0x20, "irq_rise")
      val fall_flag = factory.register(0x24, "irq_fall")
      val high_flag = factory.register(0x28, "irq_high")
      val low_flag = factory.register(0x2C, "irq_low")
      for (i <- 0 until p.width) {
        if (p.interrupt.contains(i)) {
          io.interrupt(i) := valid(i)
          rise_flag.driveAndRead(enable.rise(i), i, s"R$i") init False
          fall_flag.driveAndRead(enable.fall(i), i, s"F$i") init False
          high_flag.driveAndRead(enable.high(i), i, s"H$i") init False
          low_flag.driveAndRead(enable.low(i), i, s"L$i") init False
        } else {
          io.interrupt(i) := False
          enable.rise(i) := False
          enable.fall(i) := False
          enable.high(i) := False
          enable.low(i) := False
        }
      }
    }

    override def elements = factory.elements
    override def dataWidth = factory.dataWidth
    override def busComponentName = name
    override def wordAddressInc = factory.wordAddressInc
  }
}

case class Apb3Gpio2(
    parameter: Gpio.Parameter,
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends Gpio.Ctrl[Apb3](
      parameter,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    )
