package andreasWallner.bus.amba3.ahblite3

import andreasWallner.SpinalFunSuite
import andreasWallner.bus.amba3.ahblite3.sim.{AhbLite3ControlSignals, AhbLite3SlaveAgent, HTRANS}
import andreasWallner.sim.simLog
import spinal.core.ClockDomain
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Master}
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.{Stream, master, slave}
import spinal.lib.bus.amba4.axi.sim.SparseMemory

class AhbLite3ReadWriteDriver(ahb: AhbLite3, cd: ClockDomain) {
  ahb.HTRANS #= HTRANS.IDLE
  ahb.HSEL #= true

  forkSensitive(ahb.HREADYOUT) {
    ahb.HREADY #= ahb.HREADYOUT.toBoolean
  }

  def write(address: BigInt, data: BigInt, size: Int = 2, prot: Int = 0): Boolean = {
    ahb.HTRANS #= HTRANS.NONSEQ
    ahb.HADDR #= address
    ahb.HBURST #= 0
    ahb.HMASTLOCK #= false
    ahb.HPROT #= prot
    ahb.HSIZE #= size
    ahb.HWRITE #= true
    cd.waitSamplingWhere(ahb.HREADYOUT.toBoolean)
    ahb.HWDATA #= data
    ahb.HTRANS #= HTRANS.IDLE
    cd.waitSamplingWhere(ahb.HREADYOUT.toBoolean)
    ahb.HRESP.toBoolean
  }

  def read(address: BigInt, size: Int = 2, prot: Int = 0): BigInt = {
    ahb.HTRANS #= HTRANS.NONSEQ
    ahb.HADDR #= address
    ahb.HBURST #= 0
    ahb.HMASTLOCK #= false
    ahb.HPROT #= prot
    ahb.HSIZE #= size
    ahb.HWRITE #= false
    cd.waitSamplingWhere(ahb.HREADYOUT.toBoolean)
    ahb.HTRANS #= HTRANS.IDLE
    cd.waitSamplingWhere(ahb.HREADYOUT.toBoolean)
    ahb.HRDATA.toBigInt
  }
}

class AhbLite3MemoryAgent(ahb: AhbLite3, cd: ClockDomain, maxAddress: Option[BigInt] = None)
    extends AhbLite3SlaveAgent(ahb, cd) {
  val memory = SparseMemory()
  override def onRead(address: BigInt): (BigInt, Boolean) = {
    val bytes = (1 << addressPhase.get.size)
    val blubMask = (ahb.config.dataWidth / 8) - 1
    val accessBlubMask = bytes - 1
    assert((address & accessBlubMask) == 0, "unaligned access")
    val offset = (address.toLong & blubMask) * 8

    simLog(
      f"R ${address}%08x, ${offset}%08x, ${memory.readBigInt(address.toLong, bytes).toLong << offset}%08x"
    )

    if (maxAddress.isDefined && address > maxAddress.get) {
      (0, false)
    } else {
      (memory.readBigInt(address.toLong, bytes).toLong << offset, true)
    }
  }

  override def onWrite(address: BigInt, value: BigInt): Boolean = {
    val bytes = (1 << addressPhase.get.size)
    val subAccessMask = bytes - 1
    assert((address & subAccessMask) == 0, "unaligned access")

    if (maxAddress.isDefined && address > maxAddress.get) {
      false
    } else {
      true
    }
  }

  override def written(info: AhbLite3ControlSignals, value: BigInt, resp: Boolean): Unit = {
    if (!ahb.HRESP.toBoolean) {
      return
    }

    val bytes = (1 << addressPhase.get.size)
    val dataIndexMask = (ahb.config.dataWidth / 8) - 1
    val offset = (info.address.toLong & dataIndexMask) * 8

    val unaligned = value.toLong >> offset
    simLog(f"DW ${info.address}%08x, ${value}%08x, ${unaligned}%08x")
    memory.writeBigInt(info.address.toLong, unaligned, bytes)
  }

  def setup_descriptor(
      address: Long,
      remaining: Long,
      src_end: BigInt,
      src_inc: Boolean,
      src_size: Int,
      src_hprot: Int,
      dst_end: BigInt,
      dst_inc: Boolean,
      dst_size: Int,
      dst_hprot: Int
  ) = {
    val config = (BigInt(src_inc.toInt) << 31) | (BigInt(dst_inc.toInt) << 30) |
      (BigInt(src_size) << 27) | (BigInt(dst_size) << 24) | (BigInt(src_hprot) << 22) |
      (BigInt(dst_hprot) << 20) | BigInt(remaining)
    memory.writeBigInt(address, src_end, 4)
    memory.writeBigInt(address + 4, dst_end, 4)
    memory.writeBigInt(address + 8, config, 4)
  }

  def memrand(addr: Long, len: Int): Unit = {
    val bytes = Array.fill[Byte](len)(0)
    simRandom.nextBytes(bytes)
    simLog("Random init", addr, len)
    memory.writeArray(addr, bytes)
  }

  def memcmp(addrA: Long, addrB: Long, len: Long): Option[Long] = {
    for (offset <- 0.toLong to len) {
      if (memory.read(addrA + offset) != memory.read(addrB + offset)) {
        return Some(offset)
      }
    }
    None
  }
}

class MicroDmaTest extends SpinalFunSuite {
  val slaveConfig = spinal.lib.bus.amba3.ahblite.AhbLite3Config(16, 32)
  val masterConfig = spinal.lib.bus.amba3.ahblite.AhbLite3Config(32, 32)
  val channelCount = 1

  val dut = namedSimConfig.compile(new Component {
    val io = new Bundle {
      val sub = slave port AhbLite3(slaveConfig)
      val manager = master port AhbLite3(masterConfig)
      val handshake = Vec(master port DmaHandshake(), channelCount)
    }

    val dma = MicroDma(
      MicroDmaGenerics(channelCount, slaveConfig, masterConfig)
    )
    dma.io.sub <> io.sub
    dma.io.manager.toAhbLite3() <> io.manager
    dma.io.handshake <> io.handshake
  })

  test(dut, "WIP") { dut =>
    val driver = new AhbLite3ReadWriteDriver(dut.io.sub, dut.clockDomain)
    val mem = new AhbLite3MemoryAgent(dut.io.manager, dut.clockDomain) {
      //override def nextDelay() = 0
    }
    mem.memrand(0x40, 20)

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(1)

    for (idx <- 0x40 - 20 to 0x40) {
      mem.memory.write(idx, idx.toByte);
    }

    mem.setup_descriptor(0x00, 20, 0x40 + 20, true, 1, 0, 0x80 + 20, true, 1, 0)
    driver.write(0x04, 0x00)
    driver.write(0x00, 0x3)

    dut.clockDomain.waitSamplingWhere(dut.io.handshake(0).active.toBoolean)
    dut.clockDomain.waitSamplingWhere(!dut.io.handshake(0).active.toBoolean)
    dut.clockDomain.waitSampling(100)

    simLog(mem.memory.readArray(0x40 - 20, 20) mkString " ")
    simLog(mem.memory.readArray(0x80 - 20, 20) mkString " ")
    assert(mem.memory.readArray(0x40 - 20, 20) sameElements mem.memory.readArray(0x80 - 20, 20))
  }
}
