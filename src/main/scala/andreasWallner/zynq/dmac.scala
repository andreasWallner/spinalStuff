package andreasWallner.zynq

import andreasWallner.la.MemoryFormatter
import andreasWallner.misc.{XorShiftConfig, Xorshift}
import andreasWallner.xilinx.XilinxNamer
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}

import scala.language.postfixOps

object ZynqDma {
  object RequestType {
    def apply() = Bits(2 bits)
    def single = B"00"
    def burst = B"01"
  }

  case class Request() extends Bundle with IMasterSlave {
    val drready = Bool()
    val drlast = Bool()
    val drvalid = Bool()
    val drtype = RequestType()

    def asMaster(): Unit = {
      in(drready)
      out(drlast, drvalid, drtype)
    }

    def fire = drvalid && drready
  }
  object AckType {
    def apply() = Bits(2 bit)
    val single_ack = B"00"
    val burst_ack = B"01"
    val flush_req = B"10"
  }
  case class Ack() extends Bundle with IMasterSlave {
    val datype = Bits(2 bit)
    val davalid = Bool()
    val daready = Bool()

    def asMaster(): Unit = {
      out(datype, davalid)
      in(daready)
    }
  }

  case class ToMemory() extends Component {
    val io = new Bundle {
      val data = slave(Stream(Bits(16 bit)))
      val req = master(Request())
      val ack = slave(Ack())

      val axiStream = master(Stream(Bits(32 bit)))

      //val flush = Bool()
    }

    val fifo = StreamFifo(Bits(32 bit), 32)
    fifo.io.pop >> io.axiStream
    val formatter = MemoryFormatter(io.data, fifo.io.push)

    val unrequestedItems = Reg(UInt(8 bit)) init 0
    when(fifo.io.push.fire && io.req.fire) {
      unrequestedItems := unrequestedItems - 3
    } elsewhen(fifo.io.push.fire) {
      unrequestedItems := unrequestedItems + 1
    } elsewhen(io.req.fire) {
      unrequestedItems := unrequestedItems - 4
    }

    io.req.drlast := False
    io.req.drtype := RequestType.burst
    io.req.drvalid := unrequestedItems > 4

    io.ack.daready := True // just accept everything
  }
}

case class DmaTest(axiConfig: AxiLite4Config) extends Component {
  val io = new Bundle {
    val req = master(ZynqDma.Request())
    val ack = slave(ZynqDma.Ack())
    val axi = slave(AxiLite4(axiConfig))
  }
  val rng = Xorshift(XorShiftConfig(width=16, hasRun=false))
  val toMemory = ZynqDma.ToMemory()
  io.req <> toMemory.io.req
  io.ack <> toMemory.io.ack
  rng.io.data >> toMemory.io.data

  val factory = AxiLite4SlaveFactory(io.axi)
  factory.read(toMemory.io.axiStream.payload, 0x40000000, 0)
  toMemory.io.axiStream.ready := False
  factory.onRead(0x40000000)(
    toMemory.io.axiStream.ready := True
  )

  val generateLength = factory.createReadAndWrite(UInt(8 bit), 0x40000004, 0)
  val delayMask = factory.createReadAndWrite(Bits(8 bit), 0x40000004, 8)
  val generateTrigger = False
  factory.setOnSet(generateTrigger, 0x40000004, 16)
}

object DmaTester {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(100 MHz),
      device = Device.XILINX,
      targetDirectory = "generated"
    ).generateVerilog(
      XilinxNamer(DmaTest(AxiLite4Config(addressWidth = 32, dataWidth = 32)))
    )
  }
}
