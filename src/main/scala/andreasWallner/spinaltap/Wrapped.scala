package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.iomux.IOMux
import andreasWallner.io.pwm.Pwm
import andreasWallner.io.spi.SpiMaster
import andreasWallner.misc.BuildInfoPeripheral
import andreasWallner.io.dac.{StrobedDacGenerics, StrobedDacPeripheral}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object Wrapped {
  class BuildInfo[T <: spinal.core.Data with IMasterSlave](
      name: String,
      infoData: List[String]
  ) extends ISpinalTAPModule[T] {
    private var module: BuildInfoPeripheral[T] = null
    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new BuildInfoPeripheral[T](infoData, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
  }

  class IOMux[T <: spinal.core.Data with IMasterSlave](
      name: String,
      g: IOMux.Generics
  ) extends ISpinalTAPModule[T] {
    private var module: IOMux.Ctrl[T] = null
    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new IOMux.Ctrl[T](g, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
  }

  class Gpio[T <: spinal.core.Data with IMasterSlave](
      name: String,
      p: Gpio.Parameter
  ) extends ISpinalTAPModule[T] {
    private var module: Gpio.Ctrl[T] = null
    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new Gpio.Ctrl[T](p, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
  }

  class Pwm[T <: spinal.core.Data with IMasterSlave](
      name: String,
      p: Pwm.PeripheralParameters
  ) extends ISpinalTAPModule[T] {
    private var module: Pwm.Ctrl[T] = null
    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new Pwm.Ctrl[T](p, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
    override def otherIO() = List(module.io.pwm)
  }

  class Dac[T <: spinal.core.Data with IMasterSlave](
      name: String,
      p: StrobedDacGenerics = StrobedDacGenerics()
  ) extends ISpinalTAPModule[T] {
    private var module: StrobedDacPeripheral[T] = null
    override def init(
        busType: HardType[T],
        metaFactory: T => BusSlaveFactory
    ): Unit = {
      module = new StrobedDacPeripheral[T](p, busType, metaFactory)
      module.setName(name)
    }

    override def wrapped() = module
    override def bus() = module.io.bus
    override def otherIO() = List(module.io.dac)
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
