package andreasWallner.spinaltap

import andreasWallner.io.Gpio
import andreasWallner.io.iomux.{IOMuxGenerics, IOMuxPeripheral}
import andreasWallner.io.pwm.Pwm
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.io.TriStateArray

import scala.language.postfixOps

trait ISpinalTAPModule[T <: spinal.core.Data with IMasterSlave] {
  def bus(): T
}

trait ISpinalTAPCommModule[T <: spinal.core.Data with IMasterSlave]
    extends ISpinalTAPModule[T] {
  def triggerInputs(): List[Bool]
  def triggerOutputs(): List[Bool]
  def ports(): TriStateArray
  def vcc(): Bool
}

abstract class SpinalTap[T <: spinal.core.Data with IMasterSlave](
    busType: HardType[T],
    internalBus: HardType[T],
    moduleFactories: List[
      (HardType[T], T => BusSlaveFactory, Long) => ISpinalTAPCommModule[T]
    ],
    metaFactory: T => BusSlaveFactory,
    moduleAddressSpace: BigInt,
    interconnectFactory: (T, List[ISpinalTAPModule[T]], BigInt) => Component
) extends Component {
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

  val moduleCnt = moduleFactories.size
  val mux = new IOMuxPeripheral[T](
    IOMuxGenerics(1 + 2 + moduleCnt, 2, 5),
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

  val moduleInstances =
    for ((f, idx) <- moduleFactories.zipWithIndex)
      yield {
        val module = f(
          internalBus,
          metaFactory,
          (idx + 3) * moduleAddressSpace.longValue()
        )
        module match { case c: Component => c.setName(f"module${idx}") }
        mux.io.all(idx + 3) <> module.ports()
        for (trigger <- module.triggerInputs())
          trigger <> False
        module
      }
  val interconnect =
    interconnectFactory(
      io.bus,
      List[ISpinalTAPModule[T]](pwm, gpio0, gpio1, mux) ++ moduleInstances,
      moduleAddressSpace
    )
  // TODO logic analyzer
  // TODO trigger mux
}

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
    Apb3Decoder(
      master = bus,
      slaves = moduleMapping
    )
  }
}

class ApbSpinalTap
    extends SpinalTap[Apb3](
      Apb3(32, 32),
      Apb3(9, 32),
      List(
        (bt, mf, _) =>
          new Wrapped.Iso7816[Apb3](
            andreasWallner.io.iso7816.PeripheralGenerics(),
            bt,
            mf
          ),
        (bt, mf, _) =>
          new Wrapped.Spi[Apb3](
            andreasWallner.io.spi.PeripheralGenerics(),
            bt,
            mf
          )
      ),
      new Apb3SlaveFactory(_, 0),
      256 Byte,
      ApbSpinalTap.makeInterconnect
    )
