package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.iomux.IOMux
import andreasWallner.io.spi.SpiMaster
import andreasWallner.registers.datamodel.{Bus, BusElement, BusComponent}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.io.TriStateArray

import scala.language.postfixOps
import scala.collection.mutable.MutableList

trait ISpinalTAPModule[T <: spinal.core.Data with IMasterSlave] {
  def init(busType: HardType[T], factory: T => BusSlaveFactory): Unit

  def wrapped(): Component
  def bus(): T
  def otherIO(): List[Data] = List()
}

trait ISpinalTAPCommModule[T <: spinal.core.Data with IMasterSlave]
    extends ISpinalTAPModule[T] {
  def init(busType: HardType[T], factory: T => BusSlaveFactory): Unit

  def triggerInputs(): List[Bool] = List()

  def triggerOutputs(): List[Bool] = List()

  def ports(): TriStateArray

  def vcc(): Bool
}

abstract class SpinalTap[T <: spinal.core.Data with IMasterSlave](
    busType: HardType[T],
    internalBus: HardType[T],
    extraModules: List[ISpinalTAPModule[T]],
    commModules: List[ISpinalTAPCommModule[T]],
    metaFactory: T => BusSlaveFactory,
    moduleAddressSpace: BigInt,
    interconnectFactory: (T, List[ISpinalTAPModule[T]], BigInt) => Component,
    invertOutputEnable: Boolean = false
) extends Component
    with Bus {
  val portWidth = 5
  val io = new Bundle {
    val bus = slave(busType())

    val port0 = master(TriStateArray(portWidth))
    val port0_vcc_en = out(Bool())
    val port1 = master(TriStateArray(portWidth))
    val port1_vcc_en = out(Bool())
  }
  val portGenerics = IOMux.PortGenerics(triCnt = portWidth, outCnt = 1)
  val mux = new Wrapped.IOMux[T](
    "mux",
    IOMux.Generics(
      inPorts = 1 + 2 + commModules.size,
      outPorts = 2,
      portGenerics = portGenerics,
      invertOutputEnable = invertOutputEnable,
      withSwap = true
    )
  )
  val gpioParameter = Gpio.Parameter(
    width = portWidth + 1,
    input = (0 until portWidth).toList,
    readBufferLength = 0
  )
  val gpio0 = new Wrapped.Gpio[T]("gpio0", gpioParameter)
  val gpio1 = new Wrapped.Gpio[T]("gpio1", gpioParameter)
  val auxModules = extraModules ++ List(gpio0, gpio1, mux)

  for (module <- auxModules) {
    module.init(
      internalBus,
      metaFactory
    )
  }

  def gpioToPort(gpio: TriStateArray): IOMux.MuxedPort = {
    // map GPIO module outputs to IOMux input
    // the lower portWidth bits are I/Os and mapped
    // to tri-state signals, remaining bits to outputs
    val port = IOMux.MuxedPort(portGenerics)
    // assign seperately since TriStateArray can't be sliced
    for (i <- 0 until portWidth) {
      port.tri(i).write := gpio(i).write
      port.tri(i).writeEnable := gpio(i).writeEnable
      gpio(i).read := port.tri(i).read
    }
    port.o(0) := gpio(portWidth).write
    // read still needs to be connected to prevent compile
    // errors, even though not implemented in the register file
    // writeEnable is unused for outputs
    gpio(portWidth).read := False
    port
  }

  mux.wrapped().io.muxeds(0).tri <> io.port0
  mux.wrapped().io.muxeds(1).tri <> io.port1
  io.port0_vcc_en := mux.wrapped().io.muxeds(0).o(0)
  io.port1_vcc_en := mux.wrapped().io.muxeds(1).o(0)

  // use mux slot 0 to disable
  mux.wrapped().io.all(0).tri.writeEnable.clearAll()
  mux.wrapped().io.all(0).tri.write.clearAll()
  mux.wrapped().io.all(0).o.clearAll()

  mux.wrapped().io.all(1) <> gpioToPort(gpio0.wrapped().io.gpio)
  mux.wrapped().io.all(2) <> gpioToPort(gpio1.wrapped().io.gpio)

  for ((module, idx) <- commModules.zipWithIndex) {
    module.init(
      internalBus,
      metaFactory
    )
    mux.wrapped().io.all(idx + 3).tri <> module.ports()
    mux.wrapped().io.all(idx + 3).o(0) := module.vcc()
    for (trigger <- module.triggerInputs())
      trigger <> False
  }

  val allModules = auxModules ++ commModules
  val interconnect =
    interconnectFactory(
      io.bus,
      allModules,
      moduleAddressSpace
    )
  for (module <- allModules) {
    for (subIO <- module.otherIO) {
      val topIO = cloneOf(subIO).setPartialName(
        module.wrapped(),
        subIO.getPartialName(),
        true
      )
      topIO.copyDirectionOf(subIO)
      for ((s, t) <- (subIO.flatten zip topIO.flatten) if s.isAnalog)
        t.setAsAnalog()
      topIO <> subIO
    }
  }
  // TODO logic analyzer
  // TODO trigger mux

  /*override def elements: List[(BusElement, Long)] = allModules.zipWithIndex.flatMap { _ match {
    case (b: BusElement, idx: Int) => Some((b, 0x43c00000 + moduleAddressSpace * idx))
  }*/
  override def elements: List[(BusElement, Long)] = {
    val l = MutableList[(BusElement, Long)]()
    for ((m, idx) <- allModules.zipWithIndex) {
      m.wrapped match {
        case b: BusElement =>
          l += ((b, (0x43c00000 + moduleAddressSpace * idx).toLong))
        case _ =>
      }
    }
    l.toList
  }

  def muxConnections: List[(Int, Option[BusComponent])] = {
    val moduleConnections: List[(Int, Option[BusComponent])] =
      for ((module, idx) <- allModules.zipWithIndex)
        yield (idx, module.wrapped() match {
          case bc: BusComponent => Some(bc)
          case _                => None
        })
    moduleConnections
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

import andreasWallner.io.iso7816

class ApbSpinalTap
  extends SpinalTap[Apb3](
    Apb3(32, 32),
    Apb3(8, 32),
    List(
      new Wrapped.BuildInfo[Apb3](name = "BuildInfo", List("just", "an", "example")),
      new Wrapped.Dac[Apb3](name = "VccDac")
    ),
    List(
      new Wrapped.Iso7816[Apb3]("ISO7816", iso7816.PeripheralGenerics()),
      new Wrapped.Spi[Apb3]("SPI", SpiMaster.PeripheralParameter())
    ),
      new Apb3SlaveFactory(_, 0),
      0x100 Byte,
      ApbSpinalTap.makeInterconnect
    )
