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
import spinal.lib.com.uart.{
  UartCtrl,
  UartCtrlConfig,
  UartCtrlGenerics,
  UartCtrlInitConfig,
  UartParityType,
  UartStopType
}

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
  val fifo =
    new StreamFifo(Bits(8 bit), 256, withAsyncRead = false, withBypass = false, forFMax = true)
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

object UartCtrl2 {
  def apply(
      config: UartCtrlInitConfig,
      readonly: Boolean = false,
      generics: UartCtrlGenerics = UartCtrlGenerics()
  ): UartCtrl = {
    val uartCtrl = new UartCtrl(generics)
    uartCtrl.io.config.setClockDivider(config.baudrate Hz)
    uartCtrl.io.config.frame.dataLength := config.dataLength //8 bits
    uartCtrl.io.config.frame.parity := config.parity
    uartCtrl.io.config.frame.stop := config.stop
    uartCtrl.io.writeBreak := False
    if (readonly) {
      uartCtrl.io.write.valid := False
      uartCtrl.io.write.payload := B(0)
    }
    uartCtrl
  }
}

object iCEBreakerAsyncFifoTx
    extends iCEBreakerFlow(new iCEBreakerAsyncFifoTx(), forceDownload = true)
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

object iCEBreakerAsyncFifoRx
    extends iCEBreakerFlow(new iCEBreakerAsyncFifoRx(), forceDownload = true)
class iCEBreakerAsyncFifoRx() extends Component {
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
  af.io.tx.valid := False
  when(af.io.tx.fire) { cnt := cnt + 1 }
  af.io.rx.ready := True

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

object iCEBreakerAsyncFifoControlled
    extends iCEBreakerFlow(new iCEBreakerAsyncFifoControlled(), forceDownload = true)
class iCEBreakerAsyncFifoControlled() extends Component {
  val io = new iCEBreakerIO(
    GPIOPmod12(),
    GPIOPmod12(),
    GPIOPmod12(),
    useFifo = true
  )

  val testCtrl = TestCtrl()
  val af = AsyncFifoController(io.fifo, AsyncFifoController.Timings.FT232H)
  af.io.tx <> testCtrl.io.dataSource
  af.io.rx <> testCtrl.io.dataSink

  val uart = UartCtrl2(
    UartCtrlInitConfig(1000000, 7, UartParityType.EVEN, UartStopType.TWO),
    generics = UartCtrlGenerics(
      clockDividerWidth = log2Up(32),
      preSamplingSize = 2,
      samplingSize = 9,
      postSamplingSize = 1
    )
  )

  val overflow = Bool()
  //val toTrace = overflow ## af.io.s ## io.fifo.rd_n ## io.fifo.rxf_n ## io.fifo.wr_n ## io.fifo.txe_n ## af.io.rx.ready ## af.io.rx.valid ## af.io.tx.ready ## af.io.tx.valid ## io.fifo.d.writeEnable ## io.fifo.d.read.orR
  val toTrace = uart.io.read.valid ## uart.io.read.payload
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

  buffered.fragmentTransaction(7).translateInto(uart.io.write)((o, i) => o := i.last ## i.fragment)
  //uart.io.write <> testCtrl.io.send
  uart.io.uart.rxd := BufferCC(io.pmod1a.gpio(2).read, init = True)
  io.pmod1a.gpio(1).writeEnable := True
  io.pmod1a.gpio(1).write := uart.io.uart.txd

  uart.io.read.ready := True
  testCtrl.io.data.payload := uart.io.read.payload
  testCtrl.io.data.valid := uart.io.read.valid
}

case class TestCtrl() extends Component {
  val io = new Bundle {
    val data = slave port Flow(Bits(8 bit))

    val dataSource = master port Stream(Bits(8 bit))
    val dataSink = slave port Stream(Bits(8 bit))
    val send = master port Stream(Bits(8 bit))
  }
  val xorshift = Xorshift(XorShiftConfig(16, hasSetSeed=true))
  xorshift.io.setSeed := False

  val toSend = Reg(UInt(10 bit)) init 0
  val toReceive = Reg(UInt(10 bit)) init 0

  val firstValid = Reg(Bool()) init False
  val first = Reg(Bits(8 bit))
  val useRng = Reg(Bool()) init False
  when(io.data.valid) {
    when(~firstValid) {
      first := io.data.payload
      firstValid := True
    } otherwise {
      switch(first(7 downto 6)) {
        is(B"00") { toSend := (first(1 downto 0) ## io.data.payload).asUInt }
        is(B"01") { toReceive := (first(1 downto 0) ## io.data.payload).asUInt }
        is(B"11") {
          useRng := first(5)
          xorshift.io.setSeed := True
          toSend := 0
          toReceive := 0
        }
      }
      firstValid := False
    }
  }
  xorshift.io.seed := first ## io.data.payload

  io.send.valid.setAsReg()
  io.send.valid.clearWhen(io.send.ready).setWhen(~firstValid & io.data.valid)
  io.send.payload := first

  io.dataSource.valid := toSend =/= 0 & (~useRng | xorshift.io.data.valid)
  io.dataSource.payload := useRng.mux(xorshift.io.data.payload.resized, toSend(7 downto 0).asBits)
  xorshift.io.data.ready := io.dataSource.ready
  when(io.dataSource.fire) { toSend := toSend - 1 }

  // TODO RX data validation
  io.dataSink.ready := toReceive =/= 0
  when(io.dataSink.fire) { toReceive := toReceive - 1 }
}
