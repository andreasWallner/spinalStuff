package andreasWallner.registers.generator

import andreasWallner.registers.datamodel._
import spinal.core.{GlobalData, widthOf}
import java.io.{Writer, PrintWriter, File}

// TODO register prefix

class CHeader(intf: BusComponent, offset: Long = 0, comments: Option[Seq[String]]=None) {
  private var name = intf.busComponentName

  def overrideName(newName: String) = {
    name = newName
    this
  }

  def write(): Unit = write(f"${GlobalData.get.phaseContext.config.targetDirectory}/${name}.h")

  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
    try {
      write(writer)
    } finally {
      writer.close();
    }
  }

  def write(writer: Writer): Unit = {
    val commentLines = comments.map(_.map("# " + _)).map(_.mkString("\n") + "\n").getOrElse("")
    writer.write(s"""$commentLines#ifndef header_c_registers_${name}_h
      |#define header_c_registers_${name}_h
      |#include <stdint.h>
      |
      |#if __cplusplus
      |extern "C" {
      |#endif
      |
      |""".stripMargin)

    writeRegisterOffsets(intf, writer)
    writeBankStruct(intf, writer)
    writeRegisterStructs(intf, writer)
    writeFieldInfos(intf, writer)

    writer.write("""#if __cplusplus
      |}
      |#endif
      |#endif
      |""".stripMargin)
  }

  def writeRegisterOffsets(intf: BusComponent, writer: Writer): Unit = {
    intf.elements.foreach(_ match {
      case reg: Register => {writer.write(f"#define ${name.toUpperCase()}_${reg.name.toUpperCase()}_OFFSET 0x${reg.address + offset}%04x\n")}
    }) // TODO use correct literal macro/postfix
    writer.write("\n")
  }

  def writeBankStruct(intf: BusComponent, writer: Writer): Unit = {
    val baseType = s"uint${intf.dataWidth}_t"
    writer.write("struct {\n")
    intf.elements.flatMap { case r: Register => Some(r)
    case _ => None}.foldLeft(0L) {(expectedAddress, reg) => {
      if (reg.address != expectedAddress) {
        writer.write(s"  ${baseType} rfu_${reg.address}[${(expectedAddress - reg.address) / intf.dataWidth}];\n")
      }
      writer.write(s"  ${baseType} ${reg.name.toUpperCase()};\n")

      reg.address + intf.wordAddressInc
    }}
    writer.write(s"} ${name.toLowerCase()}_register_t;\n\n")
  }

  def writeRegisterStructs(intf: BusComponent, writer: Writer): Unit = intf.elements.foreach(_ match {
    case r: Register => writeRegisterStruct(r, intf.dataWidth, writer)
    case _ =>
  })

  def writeRegisterStruct(reg: Register, dataWidth: Long, writer: Writer): Unit = {
    val baseType = s"uint${dataWidth}_t"
    writer.write(s"typedef union {\n")
    writer.write(s"  ${baseType} v;\n")
    writer.write(s" struct {\n")
    reg.fields.foreach(field => {
      val name = field.accessType match {
        case AccessType.NA => f"rfu_${field.section.max}"
        case _ => field.name
      }
      writer.write(s"    ${baseType} ${name}:${field.section.width};\n")
    })
    writer.write(s"  } b;\n")
    writer.write(s"} ${reg.name.toLowerCase()}_t;\n\n")
  }

  def writeFieldInfos(intf: BusComponent, writer: Writer): Unit = {
    for(register <- intf.elements.flatMap { _ match { case r: Register => Some(r); case _ => None}}) {
      for(field <- register.fields if field.accessType != AccessType.NA) {
        val define = s"${name}_${register.name}_${field.name}".toUpperCase
        writer.write(s"#define ${define}_Pos ${field.section.min}\n")
        writer.write(f"#define ${define}_Msk 0x${field.section.bitmask}%x\n") // TODO use correct literal macro/postfix
        for(kv <- field.values)
          writer.write(f"#define ${define}_${kv.name.toUpperCase()} 0x${kv.value}%x\n")
      }
    }
    writer.write("\n")
  }
}
