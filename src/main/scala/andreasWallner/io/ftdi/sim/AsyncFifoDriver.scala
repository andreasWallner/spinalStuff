package andreasWallner.io.ftdi.sim

import andreasWallner.io.ftdi.AsyncFifo
import andreasWallner.sim.simCycles
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

abstract class AsyncFifoDriver(intf: AsyncFifo, cd: ClockDomain) {
  // timing requirements taken from FT2232H Datasheet, FT_000061, Version 2.6
  val T1max = 14 ns // RD# inactive to RXF#
  val T2 = 49 ns    // RXF# inactive after RD# cycle
  val T3max = 14 ns // RD# to data
  val T4 = 30 ns    // RD# min active pulse width

  val T6max = 14 ns // WR# active to TXE# inactive
  val T7 = 49 ns // TXE# inactive after WR# cycle
  val T8 = 5 ns  // data setup time
  val T9 = 20 ns // data hold time
  val T10 = 30 ns // WR# min active pulse width

  intf.rxf_n #= true
  intf.txe_n #= false

  sleep(20)

  var earliestRD: Long = -1000
  // callback to provide FTDI RX buffer state, boolean whether data is available + data
  def doRx(): Option[Int] = None
  // monitor function, called for every byte transmitted FTDI -> DUT
  def rx(bits: Int): Unit = ???
  fork {
    while (true) {
      doRx() match {
        case None => cd.waitSampling()
        case Some(toSend) =>
          rx(toSend)

          intf.rxf_n #= false
          waitUntil(!intf.rd_n.toBoolean)
          //assert(simTime() >= earliestRD, s"read: RD asserted before T2 elapsed (< ${earliestRD})")

          val afterT3 = simTime() + simCycles(T3max)
          waitUntil(simTime() >= afterT3)

          intf.d.read #= toSend
          waitUntil(intf.rd_n.toBoolean)
          val riseTime = simTime()
          //earliestRD = simTime() + simCycles(/*precharge time*/)
          intf.d.read.randomize()

          waitUntil(simTime() >= (riseTime + simCycles(T1max)) || !intf.rd_n.toBoolean)
          assert(intf.rd_n.toBoolean, "read: RD changed before T5/T2 elapsed")
          intf.rxf_n #= true

          val afterInactive = simTime() + simCycles(T2)
          waitUntil(simTime() >= afterInactive)
      }
    }
  }
  // check we don't start comm while not ready
  fork {
    while(true) {
      waitUntil(intf.rxf_n.toBoolean && !intf.rd_n.toBoolean)
      assert(assertion = false, "read: RD asserted before RXF indicates data available")
    }
  }
  // RX active pulse width checker
  fork {
    waitUntil(!intf.rd_n.toBoolean)
    val fallingEdge = simTime()
    waitUntil(intf.rd_n.toBoolean)
    assert(simTime() - fallingEdge >= simCycles(T4), "active pulse width too short")
  }

  var lastWriteChange = simTime()
  val lastWrActive: Option[Long] = None
  // keep track of last data change for setup time check
  fork {
    while (true) {
      val currentData = intf.d.write.toInt
      val currentEnable = intf.d.writeEnable.toBoolean
      waitUntil(intf.d.write.toInt != currentData || intf.d.writeEnable.toBoolean != currentEnable)
      lastWriteChange = simTime()
      lastWrActive match {
        case Some(edge) => assert(simTime() - edge >= simCycles(T9), "write: T9 hold time violation")
        case _ =>
      }
    }
  }
  // WR active pulse width checker
  fork {
    waitUntil(!intf.wr_n.toBoolean)
    val fallingEdge = simTime()
    waitUntil(intf.wr_n.toBoolean)
    assert(simTime() - fallingEdge >= simCycles(T4), "active pulse width too short")
  }
  fork {
    waitUntil(intf.wr_n.toBoolean)

    while (true) {
      waitUntil(!intf.wr_n.toBoolean)
      assert(!intf.txe_n.toBoolean, "write: started write while FTDI is indicating full buffer")
      assert(intf.d.writeEnable.toBoolean, "write: output driver not enabled")
      val setupTime = simTime() - lastWriteChange
      assert(setupTime >= simCycles(T8), f"write: T8 setup time violation {setupTime} must be > {simCycles(T8)}")
      val fallAt = simTime()

      waitUntil(intf.wr_n.toBoolean)
      //val riseAt = simTime()
      tx(intf.d.write.toInt)

      //val earliestWR = fallAt + simCycles(T7)
      //waitUntil(simTime() >= earliestWR || intf.wr_n.toBoolean)
      //val tripTime = simTime()
      //assert(
      //  tripTime >= earliestWR,
      //  s"write: T8 precharge time violation ${earliestWR} < ${tripTime}"
      //)
    }
  }
  fork {
    while (true) {
      waitUntil(intf.wr_n.toBoolean)
      waitUntil(!intf.wr_n.toBoolean)
      val fallTime = simTime()
      sleep(simCycles(T6max))
      intf.txe_n #= true
      holdTXE(fallTime)
      intf.txe_n #= false
    }
  }

  // called for every byte transmitted DUT -> FTDI
  def tx(bits: Int): Unit = ???
  // called to sleep until TXE# shall be active again after write
  def holdTXE(fallTime: Long): Unit = sleep(simCycles(T7) + Random.nextLong(simCycles(1 us)))
}
