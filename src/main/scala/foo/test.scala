package foo

import spinal.core._
import spinal.lib._

case class Dut() extends Component {
  val io = new Bundle {
    val a = in UInt(8 bits)
    val result = out UInt(8 bits)
  }
  io.result := RegNext(io.a)
}

object Test {
  def main(args: Array[String]) {
    val report = SpinalConfig().generateVerilog(Dut())
    
  }
}