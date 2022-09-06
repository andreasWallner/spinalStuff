package andreasWallner.zynq

import andreasWallner.misc.{XorShiftConfig, Xorshift}
import andreasWallner.xilinx.XilinxNamer
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}

object Dma {
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
}

case class DmaTest(axiConfig: AxiLite4Config) extends Component {
  val io = new Bundle {
    val req = master(Dma.Request())
    val ack = slave(Dma.Ack())
    val axi = slave(AxiLite4(axiConfig))
  }

  val rng = Xorshift(XorShiftConfig(width=16, hasRun=false))
  val factory = AxiLite4SlaveFactory(io.axi)
  factory.readStreamNonBlocking(rng.io.data, 0x40000000, 31, 0)

  io.req.drlast := False
  io.req.drvalid := True
  io.req.drtype := Dma.RequestType.single

  io.ack.daready := True
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
    );
  }
}
