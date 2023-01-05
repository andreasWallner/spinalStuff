package andreasWallner.registers.generator

import andreasWallner.registers.datamodel._
import spinal.core.GlobalData
import java.io.{Writer, PrintWriter, File}

class HTML(intf: BusComponent, comments: Option[Seq[String]]=None) {
  private var name = intf.busComponentName

  def overrideName(newName: String) = {
    name = newName
    this
  }

  def write(): Unit = write(f"${GlobalData.get.phaseContext.config.targetDirectory}/$name.html")

  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
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
        |          <tr class="reg $odd" align="left">
        |            <td align="center" rowspan="$fieldCnt">0x${register.address.toHexString}</td>
        |            <td align="left" rowspan="$fieldCnt">${register.name.toUpperCase()}</td>
        |            <td class="fixWidth" align="center" rowspan="$fieldCnt">${register.doc} </td>
        |            <td align="center" rowspan="$fieldCnt"></td>
        """.stripMargin)

    if(register.fields.nonEmpty) {
      writeField(register.fields.last, writer)

      register.fields.toList.reverse.tail.foreach(field => {
        writer.write(s"""</tr><tr class="$odd">""")
        writeField(field, writer)
      })
    }
    writer.write(s"""          </tr>""")
  }

  def writeField(field: Field, writer: Writer): Unit = {
    val reserved = if (field.accessType == AccessType.NA) "reserved" else ""
    writer.write(s"""
    |          <td class="$reserved" align="right">${field.section}</td>
    |          <td class="$reserved" >${field.name}</td>
    |          <td class="$reserved" align="center">${field.accessType}</td>
    |          <td class="$reserved" align="right">${formatResetValue(field.resetValue, field.section.width)}</td>
    |          <td class="$reserved fixWidth2" align="left">${field.doc.getOrElse("")}</td>
    |""".stripMargin)
  }

  val commentLines = comments.map(_.map("# " + _)).map("\n<!--\n" + _.mkString("\n") + "\n-->").getOrElse("")
  def writeHeader(writer: Writer): Unit = writer.write(s"""
      |<!DOCTYPE html>$commentLines
      |<html>
      |  <head>
      |    <title>
      |      $name
      |    </title>
      |    <style>
      |      div{
      |          text-align: center;
      |      }
      |$commonCSS
      |$Default
      |$Spring
      |    </style>
      |  </head>
      |  <body>
      |  <header align="center">
      |  <p class="regif-title"> $name Interface Document </p>
      |  </header>
      |  <div class="table">
      |  <table  align="center" class="theme-default">
      |      <br/>
      |$tableHead
      |      <tbody>
      """.stripMargin)

  def writeFooter(writer: Writer): Unit = writer.write("""
      |      </tbody>
      |  </table>
      |  </div>
      |  <footer align="center">
      |  <div> <p class="info">Powered By <a href="https://spinalhdl.github.io/SpinalDoc-RTD/"> SpinalHDL </a> </p> </div>
      |  </footer>
      |  </body>
      |</html>
      |""".stripMargin)

  val commonCSS = """
      |      body{ font-size: 0.8em; }
      |      p.regif-title{
      |          font-weight:800;
      |          font-size:1.2em;
      |      }
      |      td{
      |          white-space:pre-line; word-wrap: break-word; word-break: break-all;
      |      }
      |      td.fixWidth{
      |          min-width:50px;
      |          max-width:300px;
      |      }
      |      td.fixWidth2{
      |          min-width:50px;
      |          max-width:400px;
      |      }
      |      footer div p.info{
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
      |        <tr align="center" >
      |          <th>AddressOffset</th>
      |          <th>RegName</th>
      |          <th>Description</th>
      |          <th>Width</th>
      |          <th>Section</th>
      |          <th>FieldName</th>
      |          <th>R/W</th>
      |          <th>Reset value</th>
      |          <th>Field-Description</th>
      |        </tr>
      |      <thead>
      |""".stripMargin

  val Default =
    """
      |      .theme-default {
      |          border: 3px solid #000;
      |          border-collapse: collapse;
      |      }
      |      .theme-default td,
      |      .theme-default th{
      |          border: 1px solid #000;
      |          border-top: 1px dashed #555;
      |          border-bottom: 1px dashed #555;
      |          padding: 3px;
      |      }
      |      .theme-default th{
      |          background: #bbb;
      |      }
      |      .theme-default tbody td.reserved{
      |          color: #bbb;
      |          font-weight:200;
      |          background : #eee;
      |          /* text-decoration:line-through; */
      |          text-decoration-color:#888;
      |      }
      |      .theme-default tbody tr.reg{
      |          border-top: 2px solid #000;
      |      }
      |      .theme-default tbody tr.odd {
      |         background: #ddd
      |      }
      |""".stripMargin
  val Spring =
    """
      |      .theme-spring{
      |          border-collapse: collapse;
      |          font-size: 1.em;
      |          min-width: 800px;
      |          border-radius: 5px 5px 0 0 ;
      |          overflow: hidden;
      |          box-shadow: 0 -10px 20px rgba(0,0,0,0.15);
      |      }
      |      .theme-spring th,
      |      .theme-spring td {
      |          padding:5px 10px;
      |      }
      |      .theme-spring thead tr {
      |          background-color: #009879;
      |          color: #ffffff;
      |          text-align:center;
      |          font-weight: bold;
      |      }
      |      .theme-spring tbody tr{
      |          border-bottom: 1px solid #ddd;
      |      }
      |      .theme-spring tbody td{
      |          border: 1px solid #ddd;
      |      }
      |      .theme-spring tbody tr:last-of-type{
      |          border-bottom: 3px solid #009879;
      |      }
      |      .theme-spring tbody tr.active-row {
      |          font-weight: blod;
      |          color: #009879;
      |      }
      |      .theme-spring tbody td.reserved{
      |          color: #aaa;
      |          background : #fffff0;
      |          /* font-style:italic; */
      |          font-weight:200;
      |          font-size:1.0em;
      |      }
      |      .theme-spring tbody tr.green{
      |          background :#fffff0 ;
      |      }
      |""".stripMargin
}
