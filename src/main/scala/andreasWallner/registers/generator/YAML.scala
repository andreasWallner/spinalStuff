package andreasWallner.registers.generator

import andreasWallner.registers.datamodel._
import spinal.core.GlobalData
import java.io.{Writer, PrintWriter, File}

object StringPimper {
  implicit def richString(s: String) = new {
    def indent(steps: Int) = {
      val ind = " " * steps
      s.split("\n").map(ind + _).mkString("\n") + (if (s.endsWith("\n")) "\n"
                                                   else "")
    }

    def quoted = {
      "\"" + s + "\""
    }

    def yamlSafe = {
      s.replace("\\", "\\\\").replace("\"", "\\\"")
    }
  }
}

class YAML(comp: BusComponent) {
  import StringPimper._

  private var name = comp.busComponentName

  def overrideName(newName: String) = {
    name = newName
    this
  }

  def write(): Unit =
    write(f"${GlobalData.get.phaseContext.config.targetDirectory}/${name}.yaml")

  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
    try {
      write(writer)
    } finally {
      writer.close()
    }
  }

  def write(writer: Writer): Unit = {
    writer.write(s"""
      |---
      |!BusComponent;1
      |busComponentName: ${comp.busComponentName.yamlSafe.quoted}
      |dataWidth: ${comp.dataWidth}
      |wordAddressInc: ${comp.wordAddressInc}
      |elements:
      |""".stripMargin)

    comp.elements.map(writeElement(_, writer, 1))

    writer.write("...")
  }

  def writeElement(element: Element, writer: Writer, indent: Int): Unit = {
    element match {
      case c: Cluster  => writeCluster(c, writer, indent)
      case r: Register => writeRegister(r, writer, indent)
    }
  }

  def writeCluster(cluster: Cluster, writer: Writer, indent: Int) = {
    writer.write(f"""|-  !ElementSet;1
                     |   name: ${cluster.name.yamlSafe.quoted}
                     |   doc: ${cluster.doc.map(_.yamlSafe.quoted) getOrElse "null"}
                     |   elements:
                     |""".stripMargin)
    cluster.elements.map(writeElement(_, writer, indent + 1))
  }

  def writeRegister(register: Register, writer: Writer, indent: Int) = {
    writer.write(f"""|-  !Register;1
      |   name: ${register.name.yamlSafe.quoted}
      |   doc: ${register.doc.map(_.yamlSafe.quoted) getOrElse "null"}
      |   address: 0x${register.address}%x
      |   fields:
      |""".stripMargin.indent(indent * 3))

    register.fields.map(writeField(_, writer, indent + 1))
  }

  def writeField(field: Field, writer: Writer, indent: Int) = {
    writer.write(f"""|-  !Field;1
      |   name: ${field.name.yamlSafe.quoted}
      |   doc: ${field.doc.map(_.yamlSafe.quoted) getOrElse "null"}
      |   datatype: "${field.datatype.getClass.getName}"
      |   section: !Section;1
      |      min: ${field.section.min}
      |      max: ${field.section.max}
      |   accessType: "${field.accessType}"
      |   resetValue: ${field.resetValue}
      |   readError: ${field.readError}
      |   values:
      |""".stripMargin.indent(indent * 3))

    field.values.map(writeValue(_, writer, indent + 1))
  }

  def writeValue(value: Value, writer: Writer, indent: Int) = {
    writer.write(f"""|-  !Value;1
      |   name: ${value.name.yamlSafe.quoted}
      |   value: ${value.value}
      |   doc: ${value.doc.map(_.yamlSafe.quoted) getOrElse "null"}
      |""".stripMargin.indent(indent * 3))
  }
}
