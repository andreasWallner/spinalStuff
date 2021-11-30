package andreasWallner.spinaltap

import andreasWallner.registers.datamodel.BusComponent
import spinal.core.{GlobalData, widthOf}
import java.io.{Writer, PrintWriter, File}

object MuxConnections {
  class CppHeader(
      list: Iterable[(Int, Option[BusComponent])],
      namespace: Option[String] = None
  ) {
    def write(filename: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      try {
        write(writer)
      } finally {
        writer.close()
      }
    }

    def write(writer: Writer): Unit = {
      writer.write(s"""#ifndef header_cpp_mux_connections_hpp
      |#define header_cpp_mux_connections_hpp
      |
      |#include <cstdint>
      |#include <string_view>
      |#include <optional>
      |
      |""".stripMargin)
      if (namespace.nonEmpty)
        writer.write(s"namespace ${namespace.get} {\n")

      writer.write("enum class input : uint8_t {\n")
      for ((idx, element) <- list) {
        var name = element.map(e => e.busComponentName).getOrElse("off")
        writer.write(s"  $name = $idx,\n")
      }
      writer.write("};\n")

      writer.write("constexpr std::optional<input> from_name(std::string_view name) noexcept {\n")
      var first = true
      for (((idx, element), n) <- list.filter(x => x._2.nonEmpty).zipWithIndex) {
        var prefix = if(n == 0) "" else "else "
        var name = element.get.busComponentName
        writer.write(s"""  ${prefix}if(name == "${name}") return input::$name;\n""")
      }
      writer.write("  else return std::nullopt;\n}\n")

      if (namespace.nonEmpty)
        writer.write(s"}\n\n")
      writer.write(s"""#endif
      |""".stripMargin)
    }
  }
}
