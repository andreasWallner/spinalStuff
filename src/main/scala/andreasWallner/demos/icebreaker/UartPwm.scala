package andreasWallner.demos.icebreaker

import andreasWallner.intro.{UartApbBridge, UartApbBridgeGenerics}
import andreasWallner.io.pwm.Apb3Pwm
import andreasWallner.io.pwm.Pwm.{CoreParameters, PeripheralParameters}
import andreasWallner.registers.datamodel.{Bus, BusElement}
import spinal.core.Component
import spinal.lib.bus.amba3.apb.Apb3Config

import scala.language.postfixOps

object UartPwm extends iCEBreakerFlow(new UartPwm, forceDownload=true)

class UartPwm extends Component with Bus {
  val io = new iCEBreakerIO(GPIOPmod12(), GPIOPmod12(), iCEBreakerSnapOff())

  val apbConfig = Apb3Config(addressWidth = 5, dataWidth = 16, useSlaveError = false)

  val bridge = UartApbBridge(UartApbBridgeGenerics(apbConfig)).io
  bridge.uart <> io.uart

  val module = Apb3Pwm(
    PeripheralParameters(CoreParameters(counterWidth = 8, channelCnt = 5), dividerWidth = 16),
    apbConfig
  )
  for (i <- 1 until 4)
    io.pmod2.get.leds(i) := module.io.pwm(i)
  io.pmod2.get.led_center := module.io.pwm(0)

  bridge.apb <> module.io.bus

  def elements: Iterable[(BusElement, Long)] = Seq()//(pwm, 0))
}
