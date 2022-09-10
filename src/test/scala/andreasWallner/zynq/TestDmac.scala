package andreasWallner.zynq

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver

import scala.util.Random

case class XorShift() {
  var state = 1
  def next = {
    val old = state
    state = (state ^ (state << 7)) & 0xffff
    state = (state ^ (state >> 9)) & 0xffff
    state = (state ^ (state << 8)) & 0xffff
    old
  }
}

class TestDmac extends AnyFunSuite {
  val dut = SimConfig.withFstWave.compile(
    DmaTest(AxiLite4Config(addressWidth = 32, dataWidth = 32))
  )

  test("foo") {
    dut.doSim("foo", seed=1) { dut =>
      SimTimeout(100000)

      val axiDriver = AxiLite4Driver(dut.io.axi, dut.clockDomain)

      dut.io.req.drready #= false

      var requested = 0
      dut.clockDomain.onActiveEdges {
        if (dut.io.req.drready.toBoolean && dut.io.req.drvalid.toBoolean)
          requested += 4
        dut.io.req.drready #= requested < 32
      }

      dut.io.ack.davalid #= false
      dut.io.ack.datype #= 0

      dut.clockDomain.forkStimulus(10)
      sleep(2000)

      val xs = XorShift()
      def next2() = {
        val a = xs.next
        val b = xs.next
        (BigInt(a) << 16) + BigInt(b)
      }
      for (_ <- 1 to 100) {
        waitUntil(requested > 0)
        val r = axiDriver.read(0x40000000)
        assert(r == next2())
        dut.clockDomain.waitActiveEdge(Random.nextInt(30))
        requested -= 1
      }
    }
  }
}
