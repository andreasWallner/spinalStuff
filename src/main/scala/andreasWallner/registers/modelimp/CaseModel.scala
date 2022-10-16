package andreasWallner.registers.casemodel

import andreasWallner.registers.datamodel
import andreasWallner.registers.datamodel.{AccessType, Section}
import spinal.core.{Data, SpinalEnum, SpinalEnumElement}

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
    datatype: Data,
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
      datatype: Data,
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
      datatype: Data,
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
      datatype: Data,
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

  def apply[T <: SpinalEnum](value: SpinalEnumElement[T]): datamodel.Value =
    EnumValue(value, None)
  def apply[T <: SpinalEnum](value: SpinalEnumElement[T], doc:String): datamodel.Value =
    EnumValue(value, Some(doc))
}

// The name of an enum value is only available after the nameables have
// been named, not during initial phases of hardware elaboration
// -> delay until after nameables are named
case class EnumValue[T <: SpinalEnum](
    element: SpinalEnumElement[T],
    doc: Option[String]) extends datamodel.Value {
  override def name:String = element.getName()
  override val value = element.craft().getEncoding.getValue(element).toLong
}
