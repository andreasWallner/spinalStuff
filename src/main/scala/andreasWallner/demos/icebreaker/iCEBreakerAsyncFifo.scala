package andreasWallner.demos.icebreaker

import andreasWallner.bus.remote.{ApbMaster, ApbMasterGenerics}
import andreasWallner.iceblink.UartTransmitter
import andreasWallner.io.ftdi.AsyncFifoController
import andreasWallner.io.pwm.Apb3Pwm
import andreasWallner.io.pwm.Pwm.{CoreParameters, PeripheralParameters}
import andreasWallner.la._
import andreasWallner.misc.{XorShiftConfig, Xorshift}
import spinal.core.Component.push
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3Config

import scala.language.postfixOps

object iCEBreakerAsyncFifo extends iCEBreakerFlow(new iCEBreakerAsyncFifo(), forceDownload = true)
class iCEBreakerAsyncFifo() extends Component {
  val io = new iCEBreakerIO(
    GPIOPmod12(),
    GPIOPmod12(),
    GPIOPmod12(),
    useFifo = true
  )

  //val apbConfig = Apb3Config(addressWidth = 5, dataWidth = 16, useSlaveError = false)
  //val busMaster = ApbMaster(ApbMasterGenerics(apbConfig))
  //val xorshift = Xorshift(XorShiftConfig(16))
  val af = AsyncFifoController(io.fifo, AsyncFifoController.Timings.FT232H)
  //af.io.tx.translateFrom(xorshift.io.data){ case (y, x) => y := x.resized}
  //af.io.rx.ready := True
  val fifo = new StreamFifo(Bits(8 bit), 256, withAsyncRead=false, withBypass=false, forFMax=true)
  fifo.io.push <> af.io.rx
  fifo.io.pop <> af.io.tx
  val overflow = Bool()

  val toTrace = overflow ## af.io.s ## io.fifo.rd_n ## io.fifo.rxf_n ## io.fifo.wr_n ## io.fifo.txe_n ## af.io.rx.ready ## af.io.rx.valid ## af.io.tx.ready ## af.io.tx.valid ## io.fifo.d.writeEnable ## io.fifo.d.read.orR
  val syncedTrace = BufferCC(toTrace)
  io.pmod1b.gpio.foreach(_.writeEnable := True)
  io.pmod1b.gpio(0).write := io.fifo.rd_n
  io.pmod1b.gpio(1).write := io.fifo.rxf_n
  io.pmod1b.gpio(2).write := io.fifo.wr_n
  io.pmod1b.gpio(3).write := io.fifo.txe_n
  io.pmod1b.gpio(4).write := BufferCC(io.fifo.rxf_n)
  io.pmod1b.gpio(5).write := BufferCC(io.fifo.txe_n)
  io.pmod1b.gpio(6).write := fifo.io.push.ready
  io.pmod1b.gpio(7).write := fifo.io.pop.valid

  val compressor = RLECompressor(AnalyzerGenerics(dataWidth = toTrace.getWidth, internalWidth = 14))
  compressor.io.data := syncedTrace
  compressor.io.run := Delay(True, 10, init = False)
  val buffered = compressor.io.compressed.toStream(overflow, 2048, 2040)
  val uart = UartTransmitter(1000000)
  buffered.fragmentTransaction(7).translateInto(uart.io.data)((o, i) => o := i.last ## i.fragment)
  io.pmod1a.gpio(1).writeEnable := True
  io.pmod1a.gpio(1).write := uart.io.tx
  //af.io.rx <> busMaster.io.cmd
  //af.io.tx <> busMaster.io.resp
  //busMaster.io.abort.valid := False

  //val module = Apb3Pwm(
  //  PeripheralParameters(CoreParameters(counterWidth = 8, channelCnt = 2), dividerWidth = 16),
  //  apbConfig
  //)
  //busMaster.io.apb <> module.io.bus
  //io.redLed := module.io.pwm(0)
  //io.greenLed := module.io.pwm(1)
}

object iCEBreakerAsyncFifoTx extends iCEBreakerFlow(new iCEBreakerAsyncFifoTx(), forceDownload = true)
class iCEBreakerAsyncFifoTx() extends Component {
  val io = new iCEBreakerIO(
    GPIOPmod12(),
    GPIOPmod12(),
    GPIOPmod12(),
    useFifo = true
  )
  //val xorshift = Xorshift(XorShiftConfig(16, has))
  val af = AsyncFifoController(io.fifo, AsyncFifoController.Timings.FT232H)
  //af.io.tx.translateFrom(xorshift.io.data){ case (y, x) => y := x.resized}
  //af.io.rx.ready := True
  val cnt = Reg(UInt(8 bit)) init 0
  af.io.tx.payload := cnt.asBits
  af.io.tx.valid := True
  when(af.io.tx.fire) { cnt := cnt + 1 }
  af.io.rx.ready := False

  val overflow = Bool()
  val toTrace = overflow ## af.io.s ## io.fifo.rd_n ## io.fifo.rxf_n ## io.fifo.wr_n ## io.fifo.txe_n ## af.io.rx.ready ## af.io.rx.valid ## af.io.tx.ready ## af.io.tx.valid ## io.fifo.d.writeEnable ## io.fifo.d.read.orR
  val syncedTrace = BufferCC(toTrace)
  io.pmod1b.gpio.foreach(_.writeEnable := True)
  io.pmod1b.gpio(0).write := io.fifo.rd_n
  io.pmod1b.gpio(1).write := io.fifo.rxf_n
  io.pmod1b.gpio(2).write := io.fifo.wr_n
  io.pmod1b.gpio(3).write := io.fifo.txe_n
  io.pmod1b.gpio(4).write := BufferCC(io.fifo.rxf_n)
  io.pmod1b.gpio(5).write := BufferCC(io.fifo.txe_n)
  io.pmod1b.gpio(6).write := af.io.s(0)
  io.pmod1b.gpio(7).write := af.io.s(1)

  val compressor = RLECompressor(AnalyzerGenerics(dataWidth = toTrace.getWidth, internalWidth = 14))
  compressor.io.data := syncedTrace
  compressor.io.run := Delay(True, 10, init = False)
  val buffered = compressor.io.compressed.toStream(overflow, 2048, 2040)
  val uart = UartTransmitter(1000000)
  buffered.fragmentTransaction(7).translateInto(uart.io.data)((o, i) => o := i.last ## i.fragment)
  io.pmod1a.gpio(1).writeEnable := True
  io.pmod1a.gpio(1).write := uart.io.tx
}

