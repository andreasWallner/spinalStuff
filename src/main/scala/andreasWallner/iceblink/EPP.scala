package andreasWallner.iceblink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, BusSlaveFactoryDelayed, BusSlaveFactoryElement, SingleMapping}
import spinal.lib.io.TriState

import scala.language.postfixOps

case class EppGenerics(
    withInt:Boolean = false,
    withReset:Boolean = false) {}

/** EPP interface as provided on the ICEBlink board */
case class EPP(gen:EppGenerics = EppGenerics()) extends Bundle with IMasterSlave {
  val DB = TriState(Bits(8 bit))
  val WRITE = Bool()
  val ASTB = Bool()
  val DSTB = Bool()
  val WAIT = Bool()
  val INT = gen.withInt generate Bool()
  val RESET = gen.withReset generate Bool()

  override def asMaster(): Unit = {
    master(DB)
    out(WRITE, ASTB, DSTB)
    in(WAIT)
    if(gen.withInt) in(INT)
    if(gen.withReset) out(RESET)
  }

  override def asSlave(): Unit = {
    master(DB)
    out(WAIT)
    in(WRITE, ASTB, DSTB)
    if(gen.withInt) out(INT)
    if(gen.withReset) in(RESET)
  }

  @deprecated("not fully implemented")
  def withSync(): EPP = {
    val synced = EPP()
    if(isMasterInterface) {
      synced.asMaster()
      WRITE := synced.WRITE
      ASTB := synced.ASTB
      DSTB := synced.DSTB
      RESET := synced.DSTB

      DB.write := synced.DB.write
      DB.writeEnable := synced.DB.writeEnable
      synced.DB.read := BufferCC(DB.read)

      synced.WAIT := BufferCC(WAIT)
      synced.INT := BufferCC(INT)
    } else {
      synced.asSlave()
    }

    synced
  }
}

case class EPPStateMachine(gen:EppGenerics = EppGenerics()) extends Component {
  val io = new Bundle {
    val epp = slave(EPP(gen))
    val reg0 = out(Bits(8 bit)) setAsReg()
    val reg1 = out(Bits(8 bit)) setAsReg()
  }
  io.reg0 init 0x55
  io.reg1 init 0x77

  val syncAStb = BufferCC(io.epp.ASTB, True)
  val syncDStb = BufferCC(io.epp.DSTB, True)
  val accessOngoing = !syncAStb || !syncDStb
  val address = Reg(UInt(8 bit))

  io.epp.DB.writeEnable := io.epp.WRITE && (!io.epp.ASTB || !io.epp.DSTB)
  io.epp.DB.write setAsReg()
  io.epp.WAIT setAsReg() init False
  if(gen.withInt)
    io.epp.INT := False

  val writeData = RegNext(address.mux(
    0 -> io.reg0,
    1 -> io.reg1,
    default -> B"8'x00"
  ))
  io.epp.DB.write := io.epp.ASTB.mux(
    False -> address.asBits,
    True -> writeData
  )

  io.epp.WAIT := accessOngoing
  when(!syncDStb && !io.epp.WRITE && !io.epp.WAIT) {
    when(address === 0) {
      io.reg0 := io.epp.DB.read
    } elsewhen(address === 1) {
      io.reg1 := io.epp.DB.read
    }
  } elsewhen(!syncAStb && !io.epp.WRITE && io.epp.WAIT) {
    address := io.epp.DB.read.asUInt
  }
}

class EppBusFactory(bus: EPP) extends BusSlaveFactoryDelayed {
  override def readHalt(): Unit = ???
  override def writeHalt(): Unit = ???

  val address = Reg(UInt(8 bit))
  override def readAddress() = address
  override def writeAddress() = address

  override def busDataWidth: Int = 8
  override def wordAddressInc: Int = 1

  assert(!bus.isMasterInterface, "BusFactory can only be used to generate slaves")

  override def multiCycleRead(address: AddressMapping, cycles: BigInt): Unit = {
    assert(assertion = false, "EPP interface can't handle multi-cycle reads in the current configuration (faster single cycle reads)")
  }

  def build(): Unit = {
    val syncAStb = BufferCC(bus.ASTB, True)
    val syncDStb = BufferCC(bus.DSTB, True)
    val accessOngoing = !syncAStb || !syncDStb

    bus.DB.writeEnable := bus.WRITE && (!bus.ASTB || !bus.DSTB)
    bus.DB.write setAsReg()
    bus.WAIT setAsReg() init False
    if(bus.gen.withInt)
      bus.INT := False

    val readData = Bits(8 bit)
    readData := 0

    val doWrite = (!syncDStb && !bus.WRITE && !bus.WAIT).allowPruning()
    val doRead = (!syncDStb && bus.WRITE && !bus.WAIT).allowPruning()
    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]): Unit = super.doMappedElements(
      jobs = jobs,
      askWrite = False,
      askRead = False,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.DB.read,
      readData = readData
    )
    switch(address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }
    when(!syncAStb && !bus.WRITE && bus.WAIT) {
      address := bus.DB.read.asUInt
    }

    bus.DB.write := bus.ASTB.mux(
      False -> address.asBits,
      True -> readData
    )
    bus.WAIT := accessOngoing
  }
}
