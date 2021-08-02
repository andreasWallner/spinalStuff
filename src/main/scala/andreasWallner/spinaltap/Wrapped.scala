package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.iomux.IOMux
import andreasWallner.io.pwm.Pwm
import andreasWallner.io.spi.SpiMaster
import andreasWallner.registers.datamodel.BusComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object Wrapped {
  class IOMux[T <: spinal.core.Data with IMasterSlave](
      p: IOMux.Parameter,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends IOMux.Ctrl[T](p, busType, metaFactory)
      with ISpinalTAPModule[T] {
    override def wrapped() = this
    override def bus() = io.bus
  }

  class Gpio[T <: spinal.core.Data with IMasterSlave](
      p: Gpio.Parameter,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Gpio.Ctrl[T](p, busType, factory)
      with ISpinalTAPModule[T] {
    override def wrapped() = this
    override def bus() = io.bus
  }

  class Pwm[T <: spinal.core.Data with IMasterSlave](
      p: Pwm.PeripheralParameters,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Pwm.Ctrl[T](p, busType, factory)
      with ISpinalTAPModule[T] {
    override def wrapped() = this
    override def bus() = io.bus
  }

  class Spi[T <: spinal.core.Data with IMasterSlave](
      name: String,
      p: SpiMaster.PeripheralParameter
  ) extends ISpinalTAPCommModule[T] {
    private var module: SpiMaster.Ctrl[T] = null

    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new SpiMaster.Ctrl[T](p, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
    override def triggerInputs() = List()
    override def triggerOutputs() = List()
    override def vcc() = True

    override def ports() = {
      val tri = TriStateArray(5)
      tri(0).write := module.io.spi.mosi
      tri(0).writeEnable := True

      module.io.spi.miso := tri(1).read
      tri(1).write := False
      tri(1).writeEnable := False

      tri(2).write := module.io.spi.sclk
      tri(2).writeEnable := True

      for (i <- 3 until 5) {
        tri(i).write := False
        tri(i).writeEnable := False
      }

      tri
    }
  }

  class Iso7816[T <: spinal.core.Data with IMasterSlave](
      name: String,
      p: andreasWallner.io.iso7816.PeripheralGenerics
  ) extends ISpinalTAPCommModule[T] {
    private var module: andreasWallner.io.iso7816.Peripheral[T] = null

    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module =
        new andreasWallner.io.iso7816.Peripheral[T](p, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
    override def triggerOutputs() = List()
    override def triggerInputs() = List()
    override def ports() = {
      val tri = TriStateArray(5 bit)
      tri(0) <> module.io.iso.io

      val rstTri = tri(1)
      rstTri.write := module.io.iso.rst
      rstTri.writeEnable := True

      val clkTri = tri(2)
      clkTri.write := module.io.iso.clk
      clkTri.writeEnable := True

      for (i <- 3 until 5) {
        val rfuTri = tri(i)
        rfuTri.write := False
        rfuTri.writeEnable := False
      }

      tri
    }
    override def vcc() = module.io.iso.vcc
  }
}
