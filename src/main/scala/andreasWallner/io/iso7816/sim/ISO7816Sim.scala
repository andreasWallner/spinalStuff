package andreasWallner.io.iso7816.sim

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.core.sim._
import andreasWallner.sim._
import andreasWallner.io.iso7816.ISO7816

// TODO: measure timing of edges
case class ISO7816SimRx(io: TriState[Bool], baudrate: Double)(
    rxCallback: (Int, Boolean) => Boolean
) {
  var pullDown = false
  var baudrateLong = baudrate.asInstanceOf[Long]
  var disableRx = false

  fork {
    sleep(1)
    while (true) {
      waitUntil(!disableRx)
      rxByte(rxCallback)
    }
  }

  def rxByte(cb: (Int, Boolean) => Boolean = (_, _) => true): Int = {
    var data = 0
    var parity = false
    waitUntil(
      io.writeEnable.toBoolean && !io.write.toBoolean
    )
    sleep(baudrateLong / 2)
    for (idx <- 0 to 7) {
      sleep(baudrateLong)
      data |= (if (io.write.toBoolean) 1 else 0) << idx
      parity ^= io.write.toBoolean
    }
    sleep(baudrateLong)
    val indicateOk = cb(data, parity == io.write.toBoolean)
    sleep(baudrateLong)
    io.drive(indicateOk)
    sleep(((if (!indicateOk) 2 else 1) * baudrateLong))
    io.highz()

    data
  }
}

case class ISO7816SimTx(io: TriState[Bool], baudrate: Double)(
    txCallback: (Int, Boolean, Boolean) => Unit
) {
  var baudrateLong = baudrate.asInstanceOf[Long]

  def txByte(data: Int, induceError: Boolean = false): Boolean = {
    val bitsPull = data << 1
    var parity = (0 to 7).map(i => (data & (1 << i)) != 0).reduce((last, bit) => last ^ bit) ^ induceError
    for (idx <- 0 to 8) {
      io.drive((bitsPull & (1 << idx)) != 0)
      sleep(baudrateLong)
    }
    io.drive(parity)
    sleep(baudrateLong)
    io.highz()
    sleep(baudrateLong)
    val error = !io.read.toBoolean
    txCallback(data, error, induceError)
    sleep(2 * baudrateLong)

    error
  }
}
