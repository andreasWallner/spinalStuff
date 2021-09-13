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
    var parity = (0 to 7)
      .map(i => (data & (1 << i)) != 0)
      .reduce((last, bit) => last ^ bit) ^ induceError
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

  def tx(data: String): Boolean = {
    data
      .grouped(2)
      .map(Integer.parseUnsignedInt(_, 16))
      .foreach(x => {
        if (txByte(x))
          return true
      })
    return false
  }
}

case class ISO7816SimClient(iso: ISO7816)(
    clientFun: (ISO7816SimClient) => Unit
) {
  iso.io.simulatePullup()
  fork {
    clientFun(this)
  }

  def waitForReset() = {
    waitUntil(iso.rst.toBoolean == false)
  }

  def waitForActivation() = {
    iso.io.highz()
    waitUntil(
      iso.vcc.toBoolean == true && iso.io.read.toBoolean == true && iso.rst.toBoolean == true
    )
  }

  def send(s: String, baudrate: Double) = {
    val tx = ISO7816SimTx(iso.io, baudrate) { (_, _, _) =>
      {}
    }
    assert(tx.tx(s) == false, "Unexpected error during TX")
  }

  def rxByte(baudrate: Double): Int = {
    var data = 0
    var parity = false
    waitUntil(
      iso.io.writeEnable.toBoolean && !iso.io.write.toBoolean
    )
    sleep(baudrate.toLong / 2)
    for (idx <- 0 to 7) {
      sleep(baudrate.toLong)
      data |= (if (iso.io.write.toBoolean) 1 else 0) << idx
      parity ^= iso.io.write.toBoolean
    }
    sleep(baudrate.toLong)
    sleep(baudrate.toLong)
    iso.io.drive(true)
    sleep(((if (!true) 2 else 1) * baudrate.toLong))
    iso.io.highz()

    data
  }

  def receive(len: Int, baudrate: Double): List[Int] = {
    val x = for (idx <- 0 until len) yield rxByte(baudrate)
    x.toList
  }
}
