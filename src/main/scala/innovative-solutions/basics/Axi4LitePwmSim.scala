package innovative_solutions.basics

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.lib.bus.amba4.axilite.AxiLite4
import scala.collection.mutable.Queue

class StreamMaster[T <: Data](stream: Stream[T], clockDomain: ClockDomain) {
  val toSend = new Queue[(T) => Unit]
  val driver = StreamDriver(stream, clockDomain) { payload =>
    if (!toSend.isEmpty) {
      toSend.dequeue().apply(payload)
      true
    } else {
      false
    }
  }

  def send(func: (T) => Unit) {
    toSend += func
  }
}

class InteractiveAxiLite4Writer(axilite: AxiLite4, clockDomain: ClockDomain) {
  val wresp = new Queue[BigInt]
  val aw = new StreamMaster(axilite.aw, clockDomain)
  val w = new StreamMaster(axilite.w, clockDomain)
  val b = StreamMonitor(axilite.b, clockDomain) { payload =>
    wresp += payload.resp.toBigInt
  }
  axilite.b.ready #= true

  def write(address: BigInt, value: BigInt): Unit = {
    wresp.clear()
    aw.send({ payload =>
      payload.addr #= address
    })
    w.send({ payload =>
      payload.data #= value
      payload.strb #= 1
    })

    clockDomain.waitActiveEdgeWhere(!wresp.isEmpty)

    val resp = wresp.dequeue()
    if (resp != 0)
      throw new RuntimeException(f"write of 0x$address%04x unsuccessful, status: $resp")
  }

  val rresp = new Queue[(BigInt, BigInt)]
  val ar = new StreamMaster(axilite.ar, clockDomain)
  val r = StreamMonitor(axilite.r, clockDomain) { payload =>
    rresp += ((payload.data.toBigInt, payload.resp.toBigInt))
  }
  axilite.r.ready #= true

  def read(address: BigInt): BigInt = {
    rresp.clear()
    ar.send({ payload =>
      payload.addr #= address
    })

    clockDomain.waitActiveEdgeWhere(!rresp.isEmpty)

    val (value, resp) = rresp.dequeue()
    if(resp != 0)
      throw new RuntimeException(f"read of 0x$address%04x unsuccessful, status: $resp")
    value
  }
}

object AxiLite4PwmSim {
  def main(args: Array[String]) {
    SimConfig.withWave
      .workspacePath("/mnt/c/work/tmp/sim")
      .doSim(new AxiLite4Pwm(5)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.waitRisingEdge(10)
        val writer =
          new InteractiveAxiLite4Writer(dut.io.axilite, dut.clockDomain)
        writer.write(0x0, 0x1)
        dut.clockDomain.waitRisingEdge(100)
        val value = writer.read(0x0)
        println(value)
      }
  }
}
