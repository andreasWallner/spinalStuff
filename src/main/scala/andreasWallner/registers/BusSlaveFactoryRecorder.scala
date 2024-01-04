package andreasWallner.registers

import andreasWallner.registers.casemodel._
import andreasWallner.registers.datamodel.{AccessType, BusComponent, Section}

import scala.collection.mutable.Map
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.core.{Bits, Bool, Data}
import spinal.lib.{Flow, Stream}

import scala.collection.mutable

class RegisterRecorder(
    registers: mutable.Map[BigInt, Register],
    address: BigInt,
    factory: BusSlaveFactory
) {
  private def append(field: Field): Unit = {
    registers(address) = registers get address match {
      case None    => Register("", address.toLong, None, List(field))
      case Some(r) => r.copy(fields = r.fields :+ field)
    }
  }

  def readAndWrite[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): Unit = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RW,
        0,
        readError = false,
        Option(doc),
        values
      )
    )
    factory.readAndWrite(that, address, bitOffset, name)
  }

  def read[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RO,
        0,
        readError = false,
        Option(doc),
        values
      )
    )

    factory.read(that, address, bitOffset, name)
  }

  def createReadAndWrite[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RW,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.createReadAndWrite(that, address, bitOffset, name)
  }

  def write[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RW,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.write(that, address, bitOffset, name)
  }

  def driveAndRead[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RW,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.driveAndRead(that, address, bitOffset, name)
  }

  def doBitsAccumulationAndClearOnRead(
      that: Bits,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): Unit = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RC,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.doBitsAccumulationAndClearOnRead(that, address, bitOffset)
  }

  def setOnSet[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ) = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.WO,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.setOnSet(that, address, bitOffset)
  }

  def clearOnSet[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ) = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.WO,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.clearOnSet(that, address, bitOffset)
  }

  def readStreamNonBlocking[T <: Data](that: Stream[T], name: String): Unit = readStreamNonBlocking(that, name, null, List())

  def readStreamNonBlocking[T <: Data](
      that: Stream[T],
      name: String,
      doc: String,
      values: List[datamodel.Value]
  ): Unit = {
    append(
      Field(
        name,
        that.payload,
        Section(
          0 + that.payload.getBitsWidth - 1,
          0
        ),
        AccessType.RO,
        0,
        false,
        Option(doc),
        values
      )
    )

    factory.readStreamNonBlocking(that, address)
  }

  def readStreamNonBlocking[T <: Data](
      that: Stream[T],
      validBitOffset: Int,
      payloadBitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List(),
      validInverted: Boolean = false
  ): Unit = {
    append(
      Field(
        name,
        that.payload,
        Section(
          payloadBitOffset + that.payload.getBitsWidth - 1,
          payloadBitOffset
        ),
        AccessType.RO,
        0,
        false,
        Option(doc),
        values
      )
    )
    append(
      Field(
        name + "_valid",
        Bool(),
        Section(validBitOffset),
        AccessType.RO,
        0,
        false,
        None, // TODO
        List(
          Value(
            if (!validInverted) 0 else 1,
            "invalid",
            "Read data value is not valid"
          ),
          Value(if (!validInverted) 1 else 0, "valid", "Read data is valid")
        )
      )
    )

    factory.readStreamNonBlocking(
      that,
      address,
      validBitOffset,
      payloadBitOffset,
      validInverted
    )
  }

  def createAndDriveFlow[T <: Data](
      dataType: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): Flow[T] = {
    append(
      Field(
        name,
        dataType,
        Section(dataType.getBitsWidth + bitOffset - 1, bitOffset),
        AccessType.WO,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.createAndDriveFlow(dataType, address, bitOffset)
  }

  def onWrite(doThat: => Unit): Unit = {
    factory.onWrite(address)(doThat)
  }

  def onRead(doThat: => Unit): Unit = {
    factory.onRead(address)(doThat)
  }

  def nonStopWrite[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[datamodel.Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(that.getBitsWidth + bitOffset - 1, bitOffset),
        AccessType.WO,
        0,
        false,
        Option(doc),
        values
      )
    )
    factory.nonStopWrite(that, bitOffset, name)
  }
}

class BusSlaveFactoryRecorder(val factory: BusSlaveFactory) extends BusComponent {
  protected val registers = mutable.Map[BigInt, Register]()

  def register(
      address: BigInt,
      name: String,
      doc: String
  ): RegisterRecorder = {
    assert(!registers.contains(address), s"address $address is already used")
    assert(
      !registers.values.exists(reg => reg.name == name),
      s"Register named $name is already used"
    )
    registers(address) = Register(name, address.toLong, Option(doc), List())
    new RegisterRecorder(registers, address, factory)
  }

  def register(address: BigInt, name: String): RegisterRecorder = register(address, name, null)

  def register(
      name: String,
      doc: String
  ): RegisterRecorder = {
    assert(
      !registers.values.exists(reg => reg.name == name),
      s"Register named $name is already used"
    )
    val address = if (registers.isEmpty) BigInt(0) else registers.keys.max + factory.wordAddressInc
    register(address, name, doc)
  }

  def register(name: String): RegisterRecorder = register(name, null)

  def f = factory

  override def elements =
    registers.values.toList.sortWith((a: Register, b: Register) => a.address < b.address)

  override def busComponentName: String = ???
  override def dataWidth = factory.busDataWidth
  override def wordAddressInc: Long = factory.wordAddressInc
}
