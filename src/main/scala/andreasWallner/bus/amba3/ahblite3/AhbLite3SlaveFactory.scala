package andreasWallner.bus.amba3.ahblite3

import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryElement, SingleMapping}

import scala.collection.Seq

object AhbLite3SlaveFactory {
  def apply(bus: AhbLite3, incAddress: Int = 0) = new AhbLite3SlaveFactory(bus, incAddress)
}

class AhbLite3SlaveFactory(bus: AhbLite3, incAddress: Int = 0) extends BusSlaveFactoryDelayed {

  override def readHalt() = {}
  override def writeHalt() = {}

  override def readAddress() = bus.HADDR
  override def writeAddress() = bus.HADDR

  override def busDataWidth: Int = bus.HWDATA.getWidth
  override def wordAddressInc: Int = if (incAddress == 0) super.wordAddressInc else incAddress

  override def build(): Unit = {

    val askWrite = bus.HSEL & bus.HTRANS === 2 & bus.HWRITE
    val askRead = bus.HSEL & bus.HTRANS === 2 & !bus.HWRITE
    val doWrite = RegNext(askWrite, False)
    val doRead = RegNext(askRead, False)

    val errorFlag = (doWrite && writeErrorFlag) || (doRead && readErrorFlag)
    val errorDelay = RegNext(errorFlag) init (False)

    when(errorDelay) {
      bus.HREADYOUT := True
      bus.HRESP := True
    } elsewhen (errorFlag) {
      bus.HREADYOUT := False
      bus.HRESP := True
    } otherwise {
      bus.HREADYOUT := True
      bus.HRESP := False
    }

    val addressDelay = RegNextWhen(bus.HADDR, askRead | askWrite)

    bus.HRDATA := 0

    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.HWDATA,
      readData = bus.HRDATA
    )
    super.doNonStopWrite(bus.HWDATA)
    /** Read/Write operation */
    switch(addressDelay) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }
  }
}
