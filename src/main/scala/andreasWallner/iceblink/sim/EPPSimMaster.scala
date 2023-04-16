package andreasWallner.iceblink.sim

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import andreasWallner.iceblink.EPP
import andreasWallner.sim._

case class EPPSimMaster(epp: EPP, delay:Int) {
  epp.WRITE #= true
  epp.ASTB #= true
  epp.DSTB #= true

  def _W(value: Int, strb: Bool) = {
    epp.DB drive value
    sleep(delay)
    epp.WRITE #= false
    sleep(delay)
    strb #= false
    waitUntil(epp.WAIT.toBoolean == true)
    sleep(delay/2)
    strb #= true
    waitUntil(epp.WAIT.toBoolean == false)
    sleep(delay/2)
    epp.DB.highz()
  }

  def AW(address: Int) = {
    println(f"${simTime()} AW ${address}")
    _W(address, epp.ASTB)
  }
  def DW(data: Int) = {
    println(f"${simTime()} DW ${data}")
    _W(data, epp.DSTB)
  }

  def _R(strb: Bool): Int = {
    epp.DB.highz()
    epp.WRITE #= true
    strb #= false
    waitUntil(epp.WAIT.toBoolean == true)
    val v = epp.DB.read.toInt
    sleep(delay/2)
    strb #= true
    waitUntil(epp.WAIT.toBoolean == false)
    sleep(delay/2)

    v
  }

  def AR(): Int = {
    println(f"${simTime()} AR")
    val v = _R(epp.ASTB)
    println(f"${simTime()}    = ${v}")
    v
  }
  def DR(): Int = {
    println(f"${simTime()} DR")
    val v = _R(epp.DSTB)
    println(f"${simTime()}    = ${v}")
    v
  }

  def read(address: Int): Int = {
    AW(address)
    DR()
  }
  def write(address: Int, value: Int) = {
    AW(address)
    DW(value)
  }
}
