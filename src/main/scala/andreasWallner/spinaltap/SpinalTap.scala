package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.iomux.IOMux
import andreasWallner.io.pwm.Pwm
import andreasWallner.io.spi.SpiMaster
import andreasWallner.registers.datamodel.{Bus, BusElement}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.io.TriStateArray

import scala.language.postfixOps
import scala.collection.mutable.MutableList

trait ISpinalTAPModule[T <: spinal.core.Data with IMasterSlave] {
  def wrapped(): Component
  def bus(): T
}

trait ISpinalTAPCommModule[T <: spinal.core.Data with IMasterSlave]
    extends ISpinalTAPModule[T] {
  def init(busType: HardType[T], factory: T => BusSlaveFactory)

  def triggerInputs(): List[Bool]
  def triggerOutputs(): List[Bool]
  def ports(): TriStateArray
  def vcc(): Bool
}

abstract class SpinalTap[T <: spinal.core.Data with IMasterSlave](
    busType: HardType[T],
    internalBus: HardType[T],
    modules: List[ISpinalTAPCommModule[T]],
    metaFactory: T => BusSlaveFactory,
    moduleAddressSpace: BigInt,
    interconnectFactory: (T, List[ISpinalTAPModule[T]], BigInt) => Component
) extends Component with Bus {
  val io = new Bundle {
    val bus = slave(busType())

    val port0 = master(TriStateArray(5))
    val port1 = master(TriStateArray(5))
    val pwm = out Vec (Bool(), 3)

    val dac = new Bundle {
      val output0 = out UInt (10 bit)
      val output1 = out UInt (10 bit)
      val sel0 = out Bool ()
      val sel1 = out Bool ()
      val strobe = out Bool ()
    }
  }

  io.dac.output0.clearAll()
  io.dac.output1.clearAll()
  io.dac.sel0.clear()
  io.dac.sel1.clear()
  io.dac.strobe.clear()

  val mux = new Wrapped.IOMux[T](
    IOMux.Parameter(1 + 2 + modules.size, 2, 5),
    internalBus(),
    metaFactory
  )
  mux.io.muxeds(0) <> io.port0
  mux.io.muxeds(1) <> io.port1

  // use mux slot 0 to disable
  mux.io.all(0).writeEnable.clearAll()
  mux.io.all(0).write.clearAll()

  val gpio0 = new Wrapped.Gpio(
    Gpio.Parameter(width = 5, readBufferLength = 0),
    internalBus(),
    metaFactory
  )
  mux.io.all(1) <> gpio0.io.gpio
  val gpio1 = new Wrapped.Gpio(
    Gpio.Parameter(width = 5, readBufferLength = 0),
    internalBus(),
    metaFactory
  )
  mux.io.all(2) <> gpio1.io.gpio

  val pwm = new Wrapped.Pwm[T](
    Pwm.PeripheralParameters(Pwm.CoreParameters(channelCnt = 3)),
    internalBus(),
    metaFactory
  )
  pwm.io.pwm <> io.pwm

  for ((module, idx) <- modules.zipWithIndex) {
    module.init(
      internalBus,
      metaFactory
    )
    mux.io.all(idx + 3) <> module.ports()
    for (trigger <- module.triggerInputs())
      trigger <> False
  }
  val allModules = List[ISpinalTAPModule[T]](pwm, gpio0, gpio1, mux) ++ modules
  val interconnect =
    interconnectFactory(
      io.bus,
      allModules,
      moduleAddressSpace
    )
  // TODO logic analyzer
  // TODO trigger mux

  /*override def elements: List[(BusElement, Long)] = allModules.zipWithIndex.flatMap { _ match {
    case (b: BusElement, idx: Int) => Some((b, 0x43c00000 + moduleAddressSpace * idx))
  }*/
  override def elements: List[(BusElement, Long)] = {
    val l = MutableList[(BusElement, Long)]()
    for((m, idx) <- allModules.zipWithIndex) {
      m.wrapped match {
        case b: BusElement => l += ((b, (0x43c00000 + moduleAddressSpace * idx).toLong))
        case _ => 
      }
    }
    l.toList
  }
}

// TODO let interconnect provide list of elements
object ApbSpinalTap {
  def makeInterconnect(
      bus: Apb3,
      modules: List[ISpinalTAPModule[Apb3]],
      moduleAddressSpace: BigInt
  ): Component = {
    val moduleMapping =
      for ((m, idx) <- modules.zipWithIndex)
        yield m
          .bus() -> SizeMapping(
          0x43c00000 + moduleAddressSpace * idx,
          moduleAddressSpace
        )
    val interconnect = Apb3Decoder(
      master = bus,
      slaves = moduleMapping
    )
    interconnect
  }
}

class ApbSpinalTap
    extends SpinalTap[Apb3](
      Apb3(32, 32),
      Apb3(8, 32),
      List(
        new Wrapped.Iso7816[Apb3](
          "ISO7816",
          andreasWallner.io.iso7816.PeripheralGenerics()
        ),
        new Wrapped.Spi[Apb3]("SPI", SpiMaster.PeripheralParameter())
      ),
      new Apb3SlaveFactory(_, 0),
      256 Byte,
      ApbSpinalTap.makeInterconnect
    )
