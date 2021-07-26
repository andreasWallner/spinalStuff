package andreasWallner.io.pwm

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.amba3.apb._

import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

object Test extends SpinalEnum {
  val e1, e2, e3, e4 = newElement()
}

object Pwm {
  case class CoreParameters(
      counterWidth: Int = 32,
      channelCnt: Int = 1
  )

  case class PeripheralParameters(
      coreParameters: CoreParameters = CoreParameters(),
      prescalerWidth: Int = 32
  )

  class RegIfCtrl[T <: spinal.core.Data with IMasterSlave](
      parameters: Pwm.PeripheralParameters = Pwm.PeripheralParameters(),
      busType: HardType[T],
      generator: T => BusIf
  ) extends Component {
    val io = new Bundle {
      val bus = slave(busType())
      //val pwm = out Vec(Bool, 8)
      val run = out Bool()
    }

    val factory = generator(io.bus)
    
    val ctrl = factory.newReg(doc="Control")
    io.run := ctrl.typed(
      Bool(),
      AccessType.RW,
      doc = "State of the PWM core",
      values = List(
        KnownValue(0, "stop", Some("stopped")),
        KnownValue(1, "run", Some("running")))
    )(SymbolName("run")) // TODO warning w/o explicit name (error?) or fix it?
    val x = ctrl.typed(Test(), AccessType.RW, alignment=4)

    val conf = factory.newReg(doc="Prescaler")
    val prescaler = conf.typed(UInt(parameters.prescalerWidth bits), AccessType.RW)

    val levels = 
      for(i <- 0 until parameters.coreParameters.channelCnt)
        yield factory.newReg(doc="Reg")(SymbolName(f"level${i}")).typed(UInt(parameters.coreParameters.counterWidth bits), AccessType.RW)(SymbolName("level"))
    
    def registerBanks(): List[BusIf] = List(factory)
  }

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

    val run = mapper.createReadAndWrite(Bool, 0x00, 0) init (False)

    val prescaler = new Area {
      val set = mapper.createReadAndWrite(
        UInt(parameters.prescalerWidth bits),
        0x04,
        0
      ) init (0)
      val latched_set = Reg(UInt(parameters.prescalerWidth bits)) init (0)

      val cnt = Reg(UInt(parameters.prescalerWidth bits))
      val enable = cnt === latched_set

      when(enable || !run) {
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
    pre.core.io.run := run

    val max_count = mapper.createReadWrite(
      UInt(parameters.coreParameters.counterWidth bits),
      0x08,
      0
    ) init (0)
    val levels =
      for (i <- 0 until parameters.coreParameters.channelCnt)
        yield mapper.createReadAndWrite(
          UInt(parameters.coreParameters.counterWidth bits),
          0x0c + 4 * i,
          0
        ) init (0)

    val updateValues = (pre.core.io.willOverflow && prescaler.enable) || !run
    pre.core.io.max_count := RegNextWhen(
      max_count,
      updateValues
    ) init (0)
    for (i <- 0 until parameters.coreParameters.channelCnt)
      pre.core.io
        .levels(i) := RegNextWhen(
        levels(i),
        updateValues
      ) init (0)
  }

  case class Core(parameters: Pwm.CoreParameters) extends Component {
    val io = new Bundle {
      val max_count = in UInt (parameters.counterWidth bits)
      val levels = in Vec (UInt(parameters.counterWidth bits), parameters.channelCnt)
      val pwm = out Vec (Bool, parameters.channelCnt)
      val run = in Bool
      val willOverflow = out Bool
    }

    val counter = new Area {
      val value = Reg(UInt(parameters.counterWidth bits))
      io.willOverflow := False
      when(value === io.max_count || !io.run) {
        value := 0
        io.willOverflow := True
      } otherwise {
        value := value + 1
      }
    }

    for ((pwm, level) <- io.pwm.zip(io.levels))
      pwm := (counter.value < level) && io.run
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

case class RegIfAhbLite3Pwm(
    parameter: Pwm.PeripheralParameters = Pwm.PeripheralParameters(),
    busConfig: AhbLite3Config = AhbLite3Config(12, 32)
) extends Pwm.RegIfCtrl[AhbLite3](
      parameter,
      AhbLite3(busConfig),
      BusInterface(_, SizeMapping(0x0, 0x10))
    )