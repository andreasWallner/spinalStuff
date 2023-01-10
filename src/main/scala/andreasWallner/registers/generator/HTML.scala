package andreasWallner.registers.generator

import andreasWallner.registers.datamodel._
import spinal.core.GlobalData

import java.io.{File, PrintWriter, Writer}
import java.nio.charset.StandardCharsets

class HTML(intf: BusComponent, comments: Option[Seq[String]]=None) {
  private var name = intf.busComponentName

  def overrideName(newName: String) = {
    name = newName
    this
  }

  def write(): Unit = write(f"${GlobalData.get.phaseContext.config.targetDirectory}/$name.html")

  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename), StandardCharsets.UTF_8)
    try {
      write(writer)
    } finally {
      writer.close()
    }
  }

  def formatResetValue(value: BigInt, bitCount: Int):String = {
    val hexCount = scala.math.ceil(bitCount/4.0).toInt
    val unsignedValue = if(value >= 0) value else (BigInt(1) << bitCount) + value
    if(value == 0) s"$bitCount'b0" else s"$bitCount'h%${hexCount}s".format(unsignedValue.toString(16)).replace(' ','0')
  }

  def write(writer: Writer): Unit = {
    writeHeader(writer)
    intf.elements.foreach(writeElement(_, writer))
    writeFooter(writer)
  }

  def writeElement(element: Element, writer: Writer): Unit = {
    element match {
      case c: Cluster => writeElement(c, writer)
      case r: Register => writeRegister(r, writer)
    }
  }

  def writeRegister(register: Register, writer: Writer): Unit = {
    val fieldCnt = register.fields.size
    val odd = if(((register.address / 4) % 2) != 0) "odd" else "even"

    writer.write(s"""
        |          <tr class="reg $odd">
        |            <td class="addr" rowspan="$fieldCnt">0x${register.address.toHexString}</td>
        |            <td class="name" rowspan="$fieldCnt">${register.name.toUpperCase()}</td>
        |            <td class="doc" rowspan="$fieldCnt">${register.doc.getOrElse("&nbsp;")} </td>
        |            <td class="center" rowspan="$fieldCnt"></td>
        """.stripMargin)

    if(register.fields.nonEmpty) {
      writeField(register.fields.last, writer)

      register.fields.toList.reverse.tail.foreach(field => {
        writer.write(s"""</tr><tr class="$odd">\n""")
        writeField(field, writer)
      })
    }
    writer.write(s"""          </tr>""")
  }

  def writeField(field: Field, writer: Writer): Unit = {
    val reserved = if (field.accessType == AccessType.NA) "reserved" else ""
    writer.write(s"""
    |          <td class="section $reserved">${field.section}</td>
    |          <td class="field $reserved">${field.name}</td>
    |          <td class="accessType $reserved">${field.accessType}</td>
    |          <td class="reset $reserved">${formatResetValue(field.resetValue, field.section.width)}</td>
    |          <td class="fielddoc $reserved fixWidth2">${field.doc.getOrElse("")}</td>
    |""".stripMargin)
  }

  val commentLines = comments.map(_.map("# " + _)).map("\n<!--\n" + _.mkString("\n") + "\n-->").getOrElse("")
  def writeHeader(writer: Writer): Unit = writer.write(s"""
      |<!DOCTYPE html>$commentLines
      |<html lang="en">
      |  <head>
      |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      |    <title>$name Register Interface</title>
      |    <style>
      |$commonCSS
      |$Default
      |    </style>
      |  </head>
      |  <body>
      |  <header>
      |  <p class="title">$name Register Interface </p>
      |  </header>
      |  <table class="registers">
      |$tableHead
      |      <tbody>
      """.stripMargin)

  def writeFooter(writer: Writer): Unit = writer.write("""
      |      </tbody>
      |  </table>
      |  <footer>
      |  <p class="info">Powered by <a href="https://spinalhdl.github.io/SpinalDoc-RTD/">SpinalHDL</a></p>
      |  </footer>
      |  </body>
      |</html>
      |""".stripMargin)

  val commonCSS = """
      |      body {
      |        font-size: 0.8em;
      |        font-family: sans-serif;
      |      }
      |      p.title {
      |          font-weight:800;
      |          font-size:1.2em;
      |          text-align: center;
      |      }
      |      p.info {
      |          text-align: center;
      |      }
      |      td {
      |          white-space:pre-line; word-wrap: break-word; word-break: break-all;
      |      }
      |      td.fixWidth {
      |          min-width:50px;
      |          max-width:300px;
      |      }
      |      td.fixWidth2 {
      |          min-width:50px;
      |          max-width:400px;
      |      }
      |      footer div p.info {
      |          font-weight:300;
      |          font-size:0.7em;
      |      }
      |      a {
      |        color:black;text-decoration:none;
      |      }
      |      a:hover {
      |          color:#09f;
      |      }
      |""".stripMargin

  val tableHead = """
      |      <thead>
      |        <tr>
      |          <th>Offset</th>
      |          <th>Register</th>
      |          <th>Description</th>
      |          <th>Width</th>
      |          <th>Section</th>
      |          <th>Field</th>
      |          <th>R/W</th>
      |          <th>Reset</th>
      |          <th>Field Description</th>
      |        </tr>
      |      </thead>
      |""".stripMargin

  val Default =
    """
      |      table.registers {
      |          border-top: 3px solid #000;
      |          border-bottom: 3px solid #000;
      |          border-collapse: collapse;
      |          margin-left: auto;
      |          margin-right: auto;
      |      }
      |      table.registers td, table.registers th {
      |          border-top: 1px dashed #555;
      |          border-bottom: 1px dashed #555;
      |          padding: 3px;
      |      }
      |      table.registers th {
      |          background: #bbb;
      |      }
      |      table.registers tbody td.reserved {
      |          color: #bbb;
      |          font-weight:200;
      |          background : #eee;
      |          text-decoration-color:#888;
      |      }
      |      table.registers tbody tr.reg {
      |          border-top: 1px solid #000;
      |      }
      |      table.registers tbody tr.odd {
      |         background: #ddd;
      |      }
      |      table.registers tbody tr td.section, table.registers tbody tr td.reset {
      |         text-align: right;
      |      }
      |      table.registers tbody tr td.accessType {
      |         text-align: center;
      |      }
      |""".stripMargin
}
