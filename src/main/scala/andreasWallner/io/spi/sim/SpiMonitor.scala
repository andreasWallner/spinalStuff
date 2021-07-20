package andreasWallner.io.spi.sim

import spinal.core._
import spinal.core.sim._
import andreasWallner.io.spi.Spi

case class SpiMonitor(
    spi: Spi,
    cpha: Boolean,
    cpol: Boolean,
    msbFirst: Boolean,
    cd: ClockDomain,
    csActive: Boolean = false,
    bitsPerWord: Int = 8
)(dataCallback: (Int) => Unit) {
  val latchClk = cpol ^ !cpha
  fork {
    cd.waitActiveEdge()
    while (true) {
      waitUntil(spi.cs.toBoolean == csActive)
      assert(spi.sclk.toBoolean == cpol, "SPI clock not idle when CS is asserted")

      val word = rxWord()
      if (word.nonEmpty)
        dataCallback(word.get)
    }
  }

  def rxWord(): Option[Int] = {
    val bits = for (_ <- 0 until bitsPerWord) yield {
      if (cpha) {
        waitUntil(
          spi.sclk.toBoolean == !latchClk || spi.cs.toBoolean == !csActive
        )
        if (spi.cs.toBoolean == !csActive)
          return None
      }

      waitUntil(spi.sclk.toBoolean == latchClk || spi.cs.toBoolean == !csActive)
      if (spi.cs.toBoolean != csActive)
        return None
      val bit = spi.mosi.toBoolean

      if (!cpha) {
        waitUntil(
          spi.sclk.toBoolean == !latchClk || spi.cs.toBoolean == !csActive
        )
        if (spi.cs.toBoolean != csActive)
          return None
      }
      bit
    }
    if (msbFirst)
      Some(
        bits
          .foldLeft(0)((i: Int, b: Boolean) => (i << 1) + (if (b) 1 else 0))
      )
    else
      Some(
        bits.foldRight(0)((b: Boolean, i: Int) => (i << 1) + (if (b) 1 else 0))
      )
  }
}
