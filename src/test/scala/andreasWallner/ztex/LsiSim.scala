package andreasWallner.ztex

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.collection.mutable.Queue

case class LsiMaster(
    intf: LsiHostInterface,
    addressLen: Int = 8,
    dataLen: Int = 32
) {
  def apply(): LsiMaster = {
    intf.stop #= false
    this
  }

  def delay() = {
    sleep(100)
  }

  def send(x: BigInt, len: Int) = {
    for (i <- 0 to len - 1) {
      delay()
      intf.data.read #= ((x >> i) & 1) != 0
      delay()
      intf.clock #= !intf.clock.toBoolean
    }
  }

  def receive(len: Int) = {
    var result: BigInt = 0
    for (i <- 0 to len - 1) {
      delay()
      result += intf.data.write.toBigInt << i
      delay()
      intf.clock #= !intf.clock.toBoolean
    }
    result
  }

  def write(address: BigInt, value: BigInt) = {
    assert(address < (1 << addressLen))
    assert(value < (1 << dataLen))

    send(address, 8)
    send(value, 32)
    delay()
    intf.stop #= true
    intf.data.read #= false
    delay()
    intf.clock #= !intf.clock.toBoolean
    delay()
    intf.stop #= false
  }

  def read(address: BigInt): BigInt = {
    assert(address < (1 << addressLen))

    send(address, 8)
    delay()
    intf.stop #= true
    intf.data.read #= true
    delay()
    intf.clock #= !intf.clock.toBoolean

    receive(32)
  }
}

class MutableOption[T] {
  private[this] var value: T = _
  private[this] var loaded = false

  def get = if (loaded) value else throw new NoSuchElementException("MutableOption")
  def set(t: T): this.type = { loaded = true; value = t; this }
  def isEmpty = !loaded
  def nonEmpty = loaded
}

object LsiSim {
  def main(args: Array[String]) {
    var dut = SimConfig.withWave
      .workspacePath("/mnt/c/work/tmp/sim")
      .compile(LsiInterface())

    dut.doSim("write") { dut =>
      SimTimeout(2000 * 10)
      val master = LsiMaster(dut.io.lsi)

      dut.io.lsi.stop #= false
      dut.clockDomain.forkStimulus(10)

      fork { master.write(0xa7, 0xdeadaffe) }
      dut.clockDomain.waitActiveEdgeWhere(dut.io.mem.hw.valid.toBoolean == true)
      println(f"${dut.io.mem.hw.address.toLong}%x")
      println(f"${dut.io.mem.hw.data.toBigInt}%x")
      assert(dut.io.mem.hw.address.toLong == 0xA7L)
      assert(dut.io.mem.hw.data.toBigInt == 0xDEADAFFEL)
    }

    dut.doSim("read") { dut =>
      SimTimeout(2000 * 10)
      val master = LsiMaster(dut.io.lsi)

      dut.io.lsi.stop #= false
      dut.clockDomain.forkStimulus(10)

      val result = new MutableOption[BigInt]()
      fork { result.set(master.read(0xa7)) }

      dut.clockDomain.waitActiveEdgeWhere(dut.io.mem.hr.strb.toBoolean == true)
      assert(dut.io.mem.hr.address.toInt == 0xa7)
      dut.io.mem.hr.data #= 0xdeadaffeL

      dut.clockDomain.waitActiveEdgeWhere(result.nonEmpty)
      println(f"${result.get}%x")
      assert(result.get == 0xDEADAFFEL)
    }
  }
}
