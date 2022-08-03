package andreasWallner.io.i3c

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class I3C() extends Bundle with IMasterSlave {
  val scl, sda = TriState(Bool)

  def asMaster(): Unit = {
    master(scl, sda)
  }
}

case class I3CMaster() extends Component {
  val io = new Bundle {
    val i3c = master(I3C())
  }
}