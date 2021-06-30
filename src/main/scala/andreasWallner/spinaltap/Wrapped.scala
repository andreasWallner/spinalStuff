package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.pwm.Pwm
import andreasWallner.io.spi.SpiMasterPeripheral
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

object Wrapped {
  class Gpio[T <: spinal.core.Data with IMasterSlave](
      p: Gpio.Parameter,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Gpio.Ctrl[T](p, busType, factory)
      with ISpinalTAPModule[T] {
    override def bus() = io.bus
  }

  class Pwm[T <: spinal.core.Data with IMasterSlave](
      p: Pwm.PeripheralParameters,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Pwm.Ctrl[T](p, busType, factory)
      with ISpinalTAPModule[T] {
    override def bus() = io.bus
  }

  class Spi[T <: spinal.core.Data with IMasterSlave](
      p: andreasWallner.io.spi.PeripheralGenerics,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends SpiMasterPeripheral(p, busType, factory)
      with ISpinalTAPCommModule[T] {

    override def bus() = io.bus
    override def triggerInputs() = List()
    override def triggerOutputs() = List()
    override def vcc() = True

    override def ports() = {
      val tri = TriStateArray(5)
      tri(0).write := io.spi.mosi
      tri(0).writeEnable := True

      io.spi.miso := tri(1).read
      tri(1).write := False
      tri(1).writeEnable := False

      tri(2).write := io.spi.sclk
      tri(2).writeEnable := True

      for (i <- 3 until 5) {
        tri(i).write := False
        tri(i).writeEnable := False
      }

      tri
    }
  }

  class Iso7816[T <: spinal.core.Data with IMasterSlave](
      p: andreasWallner.io.iso7816.PeripheralGenerics,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends andreasWallner.io.iso7816.Peripheral[T](p, busType, metaFactory)
      with ISpinalTAPCommModule[T] {

    override def bus() = io.bus
    override def triggerOutputs() = List()
    override def triggerInputs() = List()
    override def ports() = {
      val tri = TriStateArray(5 bit)
      tri(0) <> io.iso.io

      val rstTri = tri(1)
      rstTri.write := io.iso.rst
      rstTri.writeEnable := True

      val clkTri = tri(2)
      clkTri.write := io.iso.clk
      clkTri.writeEnable := True

      for (i <- 3 until 5) {
        val rfuTri = tri(i)
        rfuTri.write := False
        rfuTri.writeEnable := False
      }

      tri
    }
    override def vcc() = io.iso.vcc
  }
}
