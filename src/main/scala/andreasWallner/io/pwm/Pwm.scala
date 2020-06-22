package andreasWallner.io.pwm

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.amba3.apb._

object Pwm {
  case class CoreParameters(
      counterWidth: Int = 32,
      channelCnt: Int = 1
  )

  case class PeripheralParameters(
      coreParameters: CoreParameters = CoreParameters(),
      prescalerWidth: Int = 32
  )

  abstract class Ctrl[T <: spinal.core.Data with IMasterSlave](
      parameters: Pwm.PeripheralParameters,
      busType: HardType[T],
      factory: T => BusSlaveFactory
  ) extends Component {
    val io = new Bundle {
      val bus = slave(busType())
      val pwm = out Vec (Bool, parameters.coreParameters.channelCnt)
    }

    val mapper = factory(io.bus)

    // TODO ctrl register with run/disable

    val prescaler = new Area {
      val set = mapper.createReadAndWrite(
        UInt(parameters.prescalerWidth bits),
        0x00,
        0
      ) init (0)
      val latched_set = Reg(UInt(parameters.prescalerWidth bits)) init (0)

      val cnt = Reg(UInt(parameters.prescalerWidth bits)) init (0)
      val enable = cnt === latched_set

      when(enable) {
        latched_set := set
        cnt := 0
      } otherwise {
        cnt := cnt + 1
      }
    }

    val pre = new ClockEnableArea(prescaler.enable) {
      val core = Pwm.Core(parameters.coreParameters)
    }

    io.pwm := pre.core.io.pwm

    val max_count = mapper.createReadWrite(
      UInt(parameters.coreParameters.counterWidth bits),
      0x04,
      0
    ) init (0)
    val levels =
      for (i <- 0 until parameters.coreParameters.channelCnt)
        yield mapper.createReadAndWrite(
          UInt(parameters.coreParameters.counterWidth bits),
          0x08 + 4 * i,
          0
        ) init (0)

    pre.core.io.max_count := RegNextWhen(
      max_count,
      pre.core.io.willOverflow && prescaler.enable
    )
    for (i <- 0 until parameters.coreParameters.channelCnt)
      pre.core.io
        .levels(i) := RegNextWhen(
        levels(i),
        pre.core.io.willOverflow && prescaler.enable
      )
  }

  case class Core(parameters: Pwm.CoreParameters) extends Component {
    val io = new Bundle {
      val max_count = in UInt (parameters.counterWidth bits)
      val levels = in Vec (UInt(parameters.counterWidth bits), parameters.channelCnt)
      val pwm = out Vec (Bool, parameters.channelCnt)
      val willOverflow = out Bool
    }

    val counter = new Area {
      val value = Reg(UInt(parameters.counterWidth bits))
      io.willOverflow := False
      when(value === io.max_count) {
        value := 0
        io.willOverflow := True
      } otherwise {
        value := value + 1
      }
    }

    for ((pwm, level) <- io.pwm.zip(io.levels))
      pwm := counter.value < level
  }
}

case class Apb3Pwm(
    parameter: Pwm.PeripheralParameters = Pwm.PeripheralParameters(),
    busConfig: Apb3Config = Apb3Config(12, 32)
) extends Pwm.Ctrl[Apb3](
      parameter,
      Apb3(busConfig),
      Apb3SlaveFactory(_)
    ) {}
