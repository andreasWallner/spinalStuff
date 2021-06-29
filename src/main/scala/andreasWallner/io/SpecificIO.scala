package andreasWallner.io

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

import scala.language.postfixOps

object SpecificIO {
  case class PeripheralParameter(
      inputs: Integer,
      outputs: Integer,
      width: BitCount = 32 bits,
      registerSpacing: Long = 4,
      outputOffset: Long = 0x100
  )

  class Peripheral[T <: spinal.core.Data with IMasterSlave](
      parameter: PeripheralParameter,
      busType: HardType[T],
      metaFactory: T => BusSlaveFactory
  ) extends Component {
    val io = new Bundle {
      val bus = slave(busType())
      val ins = in Vec (Bits(parameter.width), parameter.inputs)
      val outs = out Vec (Bits(parameter.width), parameter.outputs)
    }

    val factory = metaFactory(io.bus)

    assert(
      (parameter.inputs * parameter.registerSpacing) < parameter.outputOffset,
      "input and output registers must not overlap"
    )

    for ((i, idx) <- io.ins.zipWithIndex)
      factory.read(i, idx * parameter.registerSpacing, 0)
    for ((o, idx) <- io.outs.zipWithIndex)
      o := factory.createReadAndWrite(
        Bits(parameter.width),
        parameter.outputOffset + idx * parameter.registerSpacing,
        0
      ) init 0
  }
}

case class Apb3SpecificIO(
    parameter: SpecificIO.PeripheralParameter,
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends SpecificIO.Peripheral[Apb3](
      parameter,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    ) {}
