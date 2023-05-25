package andreasWallner.io.spi.sim

import spinal.core._
import spinal.core.sim._
import andreasWallner.io.spi.SpiSlave
import andreasWallner.sim.simCycles
import andreasWallner.util._

import scala.language.postfixOps

class SpiDriver(
    spi: SpiSlave,
    cpha: Boolean,
    cpol: Boolean,
    msbFirst: Boolean,
    period: Int,
    csAssertDelay: Int,
    csDeassertDelay: Int,
    cd: ClockDomain,
    bitsPerWord: Int = 8
) {

  spi.sclk #= cpol
  spi.cs #= true

  val shiftClk = cpol ^ cpha
  val latchClk = !shiftClk

  def toSeq(word: Int) = {
    (0 until bitsPerWord).map(i => (word & (1 << i)) != 0)
  }

  def writeWord(word: Int, waitAfterInitialBit: Int): Unit = {
    for ((bit, first, last) <- toSeq(word).zipWithIsFirstLast) {
      spi.mosi #= bit
      if (first)
        cd.waitSampling(waitAfterInitialBit)

      if (!cpha) {
        cd.waitSampling(period / 2)
        spi.sclk #= latchClk
        cd.waitSampling(period / 2)
        spi.sclk #= shiftClk
      } else {
        spi.sclk #= shiftClk
        cd.waitSampling(period / 2)
        spi.sclk #= latchClk
        cd.waitSampling(period / 2)
      }
    }
  }

  def write(bytes: Seq[Int]): Unit = {
    spi.cs #= false
    if (cpha)
      cd.waitSampling(csAssertDelay)

    for ((word, first) <- bytes.zipWithIsFirst) {
      writeWord(word, if (first && !cpha) csAssertDelay else 0)
    }

    cd.waitSampling(csDeassertDelay)
    spi.cs #= true
  }
}
