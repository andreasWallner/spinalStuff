package andreasWallner.io.ftdi.sim

import andreasWallner.io.ftdi.AsyncFifo
import andreasWallner.sim.simCycles
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

abstract class AsyncFifoDriver(intf: AsyncFifo, cd: ClockDomain) {
  val T6 = 80 ns
  val T2 = (50 ns) + T6
  val T3max = 50 ns
  val T5max = 25 ns

  val T7 = 50 ns
  val T8 = 50 ns
  val T9 = 20 ns
  val T11 = 25 ns // max as worst case
  val T12 = 80 ns

  intf.rxf_n #= true
  intf.txe_n #= false

  sleep(20)

  var earliestRD: Long = 0
  def doRx(): (Boolean, Int) = (false, 0)
  def rx(bits: Int): Unit = ???
  fork {
    while (true) {
      val (send, toSend) = doRx()
      if (!send) {
        cd.waitSampling()
        sleep(10)
      } else {
        rx(toSend)

        intf.rxf_n #= false
        waitUntil(!intf.rd_n.toBoolean)
        assert(simTime() >= earliestRD, s"read: RD asserted before T2 elapsed (< ${earliestRD})")

        val afterT3 = simTime() + simCycles(T3max)
        waitUntil(simTime() >= afterT3 || intf.rxf_n.toBoolean)
        assert(simTime() >= afterT3, "read: RD changed before T3/T1 was over")

        intf.d.read #= toSend
        waitUntil(intf.rd_n.toBoolean)
        val riseTime = simTime()
        earliestRD = simTime() + simCycles(T2)
        intf.d.read.randomize()

        waitUntil(simTime() >= (riseTime + simCycles(T5max)) || !intf.rd_n.toBoolean)
        assert(intf.rd_n.toBoolean, "read: RD changed before T5/T2 elapsed")
        intf.rxf_n #= true

        sleep(simCycles(T6))
      }
    }
  }
  fork {
    while(true) {
      waitUntil(intf.rxf_n.toBoolean && !intf.rd_n.toBoolean)
      assert(assertion = false, "read: RD asserted before RXF indicates data available")
    }
  }

  var lastWriteChange = simTime()
  fork {
    while (true) {
      val currentData = intf.d.write.toInt
      val currentEnable = intf.d.writeEnable.toBoolean
      waitUntil(intf.d.write.toInt != currentData || intf.d.writeEnable.toBoolean != currentEnable)
      lastWriteChange = simTime()
    }
  }

  fork {
    while (true) {
      waitUntil(intf.wr_n.toBoolean)
      assert(!intf.txe_n.toBoolean, "write: started write while FTDI is indicating full buffer")
      val riseAt = simTime()

      waitUntil(!intf.wr_n.toBoolean)
      val fallAt = simTime()
      assert(simTime() - riseAt >= simCycles(T7), "write: T7 active pulse with violation")
      assert(simTime() - lastWriteChange >= simCycles(T9), "write: T9 hold time violation")
      assert(intf.d.writeEnable.toBoolean, "write: output driver not enabled")
      tx(intf.d.write.toInt)

      val earliestWR = fallAt + simCycles(T8)
      waitUntil(simTime() >= earliestWR || intf.wr_n.toBoolean)
      val tripTime = simTime()
      sleep(5000)
      assert(
        tripTime >= earliestWR,
        s"write: T8 precharge time violation ${earliestWR} < ${tripTime}"
      )
    }
  }
  fork {
    while (true) {
      waitUntil(intf.wr_n.toBoolean)
      waitUntil(!intf.wr_n.toBoolean)
      val fallTime = simTime()
      sleep(simCycles(T11))
      intf.txe_n #= true
      holdTXE(fallTime)
      intf.txe_n #= false
    }
  }

  def tx(bits: Int): Unit = ???

  def holdTXE(fallTime: Long): Unit = sleep(simCycles(T12) + Random.nextLong(simCycles(1 us)))
}
