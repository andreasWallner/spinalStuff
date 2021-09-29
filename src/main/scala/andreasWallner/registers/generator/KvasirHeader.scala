package andreasWallner.registers.generator

import andreasWallner.registers.datamodel._
import spinal.core.{GlobalData, widthOf}
import java.io.{Writer, PrintWriter, File}
import spinal.core.Bool

class KvasirHeader(
  component: BusComponent,
  namespace: Option[String] = None,
  offset: Long = 0
) {
  private var name = component.busComponentName
  def overrideName(newName: String) = {
    name = newName
    this
  }
  def registerType = s"uint${component.dataWidth}_t"

  def write(): Unit = write(f"${GlobalData.get.phaseContext.config.targetDirectory}/${name}.kvasir.hpp")
  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
    try {
      write(writer)
    } finally {
      writer.close();
    }
  }
  def write(writer: Writer): Unit = {
    writer.write(s"""#ifndef header_kvasir_registers_${name}_h
      |#define header_kvasir_registers_${name}_h
      |
      |#include <cstdint.h>
      |#include <kvasir/Register/Utility.hpp>
      |
      |""".stripMargin)
    if(namespace.nonEmpty)
      writer.write(s"namespace ${namespace.get} {\n")

    component.elements.map(writeElement(_, writer))

    if(namespace.nonEmpty)
      writer.write(s"}\n")
    writer.write(s"""#endif
      |""".stripMargin)
  }

  def writeElement(element: Element, writer: Writer): Unit = {
    element match {
      case c: Cluster => c.elements.map(writeElement(_, writer))
      case r: Register => writeRegister(r, writer)
    }
  }

  def accessTypeStr(field: Field): String =
    field.accessType match {
      case AccessType.RO => "Register::ReadOnlyAccess"
      case AccessType.RW => "Register::ReadWriteAccess"
      case AccessType.RC => "Register::Access<AccessType::readOnly, ReadActionType::clear>"
      case AccessType.RS => "Register::Access<AccessType::readOnly, ReadActionType::set>"
      case AccessType.WRC => "Register::Access<AccessType::readWrite, ReadActionType::clear>"
      case AccessType.WRS => "Register::Access<AccessType::readWrite, ReadActionType::set>"
      case AccessType.WC => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::clear>"
      case AccessType.WS => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::set>"
      case AccessType.WSRC => "Register::Access<AccessType::readWrite, ReadActionType::clear, ModifiedWriteValueType::set>"
      case AccessType.WCRS => "Register::Access<AccessType::readWrite, ReadActionType::set, ModifiedWriteValueType::clear>"
      case AccessType.W1C => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::oneToClear>"
      case AccessType.W1S => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::oneToSet>"
      case AccessType.W1T => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::oneToToggle>"
      case AccessType.W0C => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::zeroToClear>"
      case AccessType.W0S => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::zeroToSet>"
      case AccessType.W0T => "Register::Access<AccessType::readWrite, ReadActionType::normal, ModifiedWriteValueType::zeroToToggle>"
      case AccessType.W1SRC => "Register::Access<AccessType::readWrite, ReadActionType::clear, ModifiedWriteValueType::oneToSet>"
      case AccessType.W1CRS => "Register::Access<AccessType::readWrite, ReadActionType::set, ModifiedWriteValueType::oneToClear>"
      case AccessType.W0SRC => "Register::Access<AccessType::readWrite, ReadActionType::clear, ModifiedWriteValueType::zeroToSet>"
      case AccessType.W0CRS => "Register::Access<AccessType::readWrite, ReadActionType::set, ModifiedWriteValueType::zeroToClear>"
      case AccessType.WO => "Register::Access<AccessType::writeOnly>"
      case AccessType.WOC => "Register::Access<AccessType::writeOnly, ReadActionType::normal, ModifiedWriteValueType::clear>"
      case AccessType.WOS => "Register::Access<AccessType::writeOnly, ReadActionType::normal, ModifiedWriteValueType::set>"
      case AccessType.W1 => "Register::Access<AccessType::readWriteOnce>"
      case AccessType.WO1 => "Register::Access<AccessType::writeOnce>"
      case AccessType.W1P => "Register::Access<AccessType::readWrite>"
      case AccessType.W0P => "Register::Access<AccessType::readWrite>"
      case _ => throw new Exception(s"unknown how to handle access type ${field.accessType}")
    }

  def writeOneIgnore(accessType: AccessType): Boolean =
    accessType match {
      case AccessType.RO => true
      case AccessType.RW => false
      case AccessType.RC => true
      case AccessType.RS => true
      case AccessType.WRC => false
      case AccessType.WRS => false
      case AccessType.WC => false
      case AccessType.WS => false
      case AccessType.WSRC => false
      case AccessType.WCRS => false
      case AccessType.W1C => false
      case AccessType.W1S => false
      case AccessType.W1T => false
      case AccessType.W0C => true
      case AccessType.W0S => true
      case AccessType.W0T => true
      case AccessType.W1SRC => false
      case AccessType.W1CRS => false
      case AccessType.W0SRC => true
      case AccessType.W0CRS => true
      case AccessType.WO => false
      case AccessType.WOC => false
      case AccessType.WOS => false
      case AccessType.W1 => false
      case AccessType.WO1 => false
      case AccessType.W1P => false
      case AccessType.W0P => false
      case _ => throw new Exception(s"unknown how to handle access type ${accessType}")
    }

  def writeZeroIgnore(accessType: AccessType): Boolean =
    accessType match {
      case AccessType.RO => true
      case AccessType.RW => false
      case AccessType.RC => true
      case AccessType.RS => true
      case AccessType.WRC => false
      case AccessType.WRS => false
      case AccessType.WC => false
      case AccessType.WS => false
      case AccessType.WSRC => false
      case AccessType.WCRS => false
      case AccessType.W1C => true
      case AccessType.W1S => true
      case AccessType.W1T => true
      case AccessType.W0C => false
      case AccessType.W0S => false
      case AccessType.W0T => false
      case AccessType.W1SRC => true
      case AccessType.W1CRS => true
      case AccessType.W0SRC => false
      case AccessType.W0CRS => false
      case AccessType.WO => false
      case AccessType.WOC => false
      case AccessType.WOS => false
      case AccessType.W1 => false
      case AccessType.WO1 => false
      case AccessType.W1P => false
      case AccessType.W0P => false
      case _ => throw new Exception(s"unknown how to handle access type ${accessType}")
    }

  def usedMask(register: Register): Long = register.fields.map(f => if(f.accessType != AccessType.NA) f.section.bitmask else 0).reduce((a, b) => a | b)
  def unusedMask(register: Register): Long = Section(component.dataWidth.toInt - 1, 0).bitmask ^ usedMask(register)
  def ignoreMask(register: Register, pred: AccessType => Boolean): Long =
    unusedMask(register) | register.fields.map(f => if(pred(f.accessType)) f.section.bitmask else 0).reduce((a, b) => a | b)

  def writeRegister(register: Register, writer: Writer): Unit = {
    if(register.doc.nonEmpty)
      writer.write(s"/// ${register.doc.getOrElse("")}")
    writer.write(s"namespace ${register.name} {\n")
    writer.write(f"using Addr = Register::Address<" +
      f"0x${register.address + offset}%xU, " +
      f"0x${ignoreMask(register, writeOneIgnore)}%xU, " +
      f"0x${ignoreMask(register, writeZeroIgnore)}%xU, " +
      f"${registerType}>;\n")

    for(field <- register.fields.filter(f => f.accessType != AccessType.NA)) {
      val fieldType = if(field.values.nonEmpty) {
        writer.write(s"enum class ${field.name}Val {\n")
        for(value <- field.values) {
          if(value.doc.nonEmpty)
            writer.write(s"/// ${value.doc.get}\n")
          writer.write(s"${value.name}=${value.value},\n")
        }
        writer.write(s"};\n")
        field.name + "Val"
      } else {
        "unsigned"
      }

      if(field.doc.nonEmpty)
        writer.write(s"/// ${field.doc.get}\n")
      writer.write(s"constexpr Register::FieldLocation<" +
        s"Addr, " +
        s"Register::maskFromRange(${field.section.max}, ${field.section.min}), " +
        s"${accessTypeStr(field)}, " +
        s"${fieldType}> " +
        s"${field.name} {};\n")

      if(field.values.nonEmpty) {
        writer.write(s"namespace ${fieldType}C {\n")
        for(value <- field.values) {
          writer.write(s"constexpr Register::FieldValue<" +
            s"decltype(${field.name}::Type, " +
            s"${fieldType}::${value.name}> " +
            s"${value.name} {};\n")
        }
        writer.write(s"}\n")
      }
    }

    writer.write(s"}\n")
  }
}
