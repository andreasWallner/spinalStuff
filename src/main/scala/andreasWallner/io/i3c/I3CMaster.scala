package andreasWallner.io.i3c

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class I3C(slaveOnly: Boolean = false) extends Bundle with IMasterSlave {
  val scl, sda = TriState(Bool())
  val pullup = if(!slaveOnly) Some(Bool()) else None
  val keeper = if(!slaveOnly) Some(Bool()) else None

  def asMaster(): Unit = {
    master(scl, sda)
    pullup.map(b => out(b))
    keeper.map(b => out(b))
  }
}

case class I3CMaster() extends Component {
  val io = new Bundle {
    val i3c = master(I3C())
  }
}
