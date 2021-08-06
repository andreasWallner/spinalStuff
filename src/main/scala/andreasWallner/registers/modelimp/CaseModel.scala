package andreasWallner.registers.casemodel

import andreasWallner.registers.datamodel
import andreasWallner.registers.datamodel.{Section, AccessType}
import spinal.core.{HardType, Data}

case class Register(
    name: String,
    address: Long,
    doc: Option[String],
    fields: List[Field]
) extends datamodel.Register {}
object Register {
  def apply(
      name: String,
      address: Long,
      doc: String,
      fields: List[Field]
  ): Register = Register(name, address, Some(doc), fields)
}

case class Field(
    name: String,
    datatype: HardType[Data],
    section: Section,
    accessType: AccessType,
    resetValue: Long,
    readError: Boolean,
    doc: Option[String],
    values: List[datamodel.Value]
) extends datamodel.Field {}
object Field {
  def apply(
      name: String,
      datatype: HardType[Data],
      section: Section,
      accessType: AccessType,
      resetValue: Long,
      readError: Boolean,
      doc: String,
      value: List[datamodel.Value]
  ): Field =
    Field(
      name,
      datatype,
      section,
      accessType,
      resetValue,
      readError,
      Some(doc),
      value
    )
  def apply(
      name: String,
      datatype: HardType[Data],
      section: Section,
      accessType: AccessType,
      resetValue: Long,
      readError: Boolean,
      doc: String
  ): Field =
    Field(
      name,
      datatype,
      section,
      accessType,
      resetValue,
      readError,
      Some(doc),
      List()
    )
  def apply(
      name: String,
      datatype: HardType[Data],
      section: Section,
      accessType: AccessType,
      resetValue: Long,
      readError: Boolean
  ): Field =
    Field(
      name,
      datatype,
      section,
      accessType,
      resetValue,
      readError,
      None,
      List()
    )
}

case class Value(
    value: Long,
    name: String,
    doc: Option[String]
) extends datamodel.Value {}
object Value {
  def apply(value: Long, name: String, doc: String): Value =
    Value(value, name, Some(doc))
  def apply(value: Long, name: String): Value = Value(value, name, None)
}
