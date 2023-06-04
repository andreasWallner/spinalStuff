package andreasWallner.io.uart.sim

import spinal.core._
import spinal.core.sim._
import andreasWallner.Utils.{evenParity, oddParity}

class UartSimDriver(
    pin: Bool,
    baudPeriod: Long,
    parity: String = "odd",
    stopBits: Double = 2.0
) {
  assert(baudPeriod > 0, f"number of cycles per period must be > 0, not $baudPeriod")
  pin #= true

  def send(b: Byte): Unit = {
    pin #= false
    sleep(baudPeriod)
    for (i <- 0 to 7) {
      pin #= ((b >> i) & 1) == 1
      sleep(baudPeriod)
    }
    parity match {
      case "odd"   => pin #= oddParity(b)
      case "even"  => pin #= evenParity(b)
      case "mark"  => pin #= true
      case "space" => pin #= false
    }
    sleep(baudPeriod)

    pin #= true
    sleep(stopBits * baudPeriod)
  }

  def send(x: BigInt, idleTime: () => Double = () => 0.0): Unit = {
    x.toBytes(endian = BIG).foreach { b =>
      send(b)
      sleep(idleTime())
    }
  }

  def break(cycles: Long, postCycles: Long = 2 * baudPeriod): Unit = {
    pin #= false
    sleep(cycles)
    pin #= true
    sleep(postCycles)
  }
}
