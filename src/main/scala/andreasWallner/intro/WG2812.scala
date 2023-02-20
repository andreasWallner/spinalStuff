package andreasWallner.intro

import andreasWallner.ext.PimpedPhysicalNumber
import spinal.core._

import scala.language.postfixOps

object WG2812 {
  def validateTimings(shortCnt: BigInt, longCnt: BigInt, freq: HertzNumber): Unit = {
    val shortTime = freq.toTime * BigDecimal(shortCnt)
    val longTime = freq.toTime * BigDecimal(longCnt)
    assert(
      shortTime > (0.3 us),
      f"short time not within spec (${shortTime.decomposeString} < 0.3us)"
    )
    assert(
      shortTime < (0.55 us),
      f"short time not within spec (${shortTime.decomposeString} > 0.55us)"
    )
    assert(longTime > (0.7 us), f"long time not within spec (${longTime.decomposeString} < 0.7us)")
    assert(longTime < (1.0 us), f"long time not within spec (${longTime.decomposeString} > 1.0us)")
  }

  def calculateTimings(
      freq: HertzNumber = ClockDomain.current.frequency.getValue,
      validate: Boolean = true
  ) = {
    val rstCnt = ((50 us) * freq).toBigInt()
    val shortCnt = ((.425 us) * freq).toBigInt()
    val longCnt = ((.825 us) * freq).toBigInt()

    if (validate)
      validateTimings(shortCnt, longCnt, ClockDomain.current.frequency.getValue)

    (rstCnt, shortCnt, longCnt)
  }
}
