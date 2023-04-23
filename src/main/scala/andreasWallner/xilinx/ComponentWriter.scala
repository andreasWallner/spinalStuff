package andreasWallner.xilinx

import spinal.core._

import scala.collection.mutable

case class PortMap(logicalPort: String, physicalPort: Data)

object Helpers {
  implicit class PimpedStringBuilder(val sb: mutable.StringBuilder) {
    def ++=(pm: PortMap) = {
      sb ++= s"""
                |       <spirit:portMap>
                |         <spirit:logicalPort>
                |           <spirit:name>${pm.logicalPort}</spirit:name>
                |         </spirit:logicalPort>
                |         <spirit:physicalPort>
                |           <spirit:name>${pm.physicalPort
                  .getName()}</spirit:name>
                |         </spirit:physicalPort>
                |       </spirit:portMap>
    """.stripMargin
    }
  }
}

object VivadoIpify {
  def emitComponentXml(that: Component): Unit = {
    val out = new java.io.FileWriter("component.xml")
    val builder = new mutable.StringBuilder()

    genComponentXml(that, builder, "andreasWallner", "user", "v1.0")
    out.write(builder.toString)
    out.flush()
    out.close()
  }

  def genComponentXml(
      that: Component,
      builder: mutable.StringBuilder,
      vendor: String,
      library: String,
      version: String
  ): mutable.StringBuilder = {
    val name = that.definitionName
    builder ++= s"""
                   |<?xml version="1.0" encoding="UTF-8"?>
                   |<spirit:component xmlns:xilinx="http://www.xilinx.com" xmlns:spirit="http://www.spiritconsortium.org/XMLSchema/SPIRIT/1685-2009" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                   | <spirit:vendor>$vendor</spirit:vendor>
                   | <spirit:library>$library</spirit:library>
                   | <spirit:name>$name</spirit:name>
                   | <spirit:version>$version</spirit:version>
      """.stripMargin

    genBusInterfaces(that, builder)

    builder ++= "<spirit:memoryMaps>\n"
    builder ++= "</spirit:memoryMaps>\n"

    builder ++= "<spirit:description>\n"
    builder ++= "</spirit:description>\n"

    builder ++= s"""
                   |</spirit:component>
                   |""".stripMargin
  }

  def genBusInterfaces(
      that: Component,
      builder: mutable.StringBuilder
  ): mutable.StringBuilder = {
    builder ++= "  <spirit:busInterfaces>\n"

    forMasterSlaveInterfaces(that) { (ms, idx) =>
      XilinxSupportFactory.makeSupport(ms) match {
        case Some(support) => genBusInterface(support, idx); true
        case None          => false
      }
    }

    def genBusInterface(support: BusSupport, idx: Int): Unit = {
      import Helpers._
      import support.{abstractionType, busType}

      val name = support.name(idx)

      builder ++= "    <spirit:busInterface>\n"
      builder ++= s"""
                     |      <spirit:name>$name</spirit:name>
                     |      <spirit:busType spirit:vendor="${busType.vendor}" spirit:library="${busType.library}" spirit:name="${busType.name}" spirit:version="${busType.version}"/>
                     |      <spirit:abstractionType spirit:vendor="${abstractionType.vendor}" spirit:library="${abstractionType.library}" spirit:name="${abstractionType.name}" spirit:version="${abstractionType.version}"/>
                     |      <spirit:slave>
                     |        <spirit:memoryMapRef spirit:memoryMapRef="$name"/>
                     |      </spirit:slave>
                     |      <spirit:portMaps>""".stripMargin

      support.portMaps().foreach(map => builder ++= map)

      builder ++= s"""
                     |     </spirit:portMaps>
                     |""".stripMargin
      builder ++= "    </spirit:busInterface>\n"
    }

    builder ++= "  </spirit:busInterface>\n"
  }
}

case class SpiritBusType(
    vendor: String,
    library: String,
    name: String,
    version: String
)
case class SpiritAbstractionType(
    vendor: String,
    library: String,
    name: String,
    version: String
)
