package andreasWallner.registers

import andreasWallner.registers.casemodel._
import andreasWallner.registers.datamodel.{AccessType, Section, BusComponent}
import scala.collection.mutable.Map
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.core.{Data, Bits, Bool}
import spinal.lib.{Stream, Flow}

class RegisterRecorder(
    registers: Map[BigInt, Register],
    address: BigInt,
    factory: BusSlaveFactory
) {
  private def append(field: Field) = {
    registers(address) = registers get address match {
      case None    => Register("", address.toLong, None, List(field))
      case Some(r) => r.copy(fields = r.fields :+ field)
    }
  }

  def read[T <: Data](
      that: T,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[Value] = List()
  ): T = {
    append(
      Field(
        name,
        that,
        Section(bitOffset + that.getBitsWidth - 1, bitOffset),
        AccessType.RO,
        0,
        false,
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
      values: List[Value] = List()
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

  def doBitsAccumulationAndClearOnRead(
      that: Bits,
      bitOffset: Int,
      name: String,
      doc: String = null,
      values: List[Value] = List()
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
      values: List[Value] = List()
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

  def readStreamNonBlocking[T <: Data](
      that: Stream[T],
      validBitOffset: Int,
      payloadBitOffset: Int,
      name: String,
      doc: String = null,
      values: List[Value] = List(),
      validInverted: Boolean = false
  ): Unit = {
    append(
      Field(
        name,
        that.payload,
        Section(payloadBitOffset + that.payload.getBitsWidth - 1, payloadBitOffset),
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
        Section(payloadBitOffset),
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
      values: List[Value] = List()
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
}

class BusSlaveFactoryRecorder(factory: BusSlaveFactory) extends BusComponent {
  protected val registers = Map[BigInt, Register]()

  def register(
      address: BigInt,
      name: String,
      doc: String = null
  ): RegisterRecorder = {
    assert(!registers.contains(address))
    registers(address) = Register(name, address.toLong, Option(doc), List())
    new RegisterRecorder(registers, address, factory)
  }

  def f = factory

  override def elements =
    registers.values.toList.sortWith((a: Register, b: Register) =>
      a.address < b.address
    )
  override def busComponentName: String = ???
  override def dataWidth = factory.busDataWidth
  override def wordAddressInc: Long = factory.wordAddressInc
}
