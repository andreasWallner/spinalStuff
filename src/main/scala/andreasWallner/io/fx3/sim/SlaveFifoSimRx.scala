package andreasWallner.io.fx3.sim

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.Queue
import andreasWallner.io.fx3._

case class SimHistory[T](depth: Int, initial: T = None) {
  var data: Seq[T] = List.fill(depth)(initial)

  def apply(idx: Int) = data(idx)
  def update(next: T) = {
    data = List(next) ++ data.slice(0, depth - 1)
  }
}

case class SlaveFifoSimRx(intf: SlaveFifo, clockDomain: ClockDomain)(
    rxCallback: (Int) => Unit
) {
  var pktendCallback: (Queue[Int]) => Unit = (_) => {}
  var next_remaining_space: () => Int = () => {
    // use a minimum of 4 as worst case
    // in practice the memory is much bigger, but smaller
    // values produce weird special cases in simulation that
    // can't happen in reality
    // (having to retransmit stuff from retransmit buffer)
    Random.nextInt(10) + 4
  }
  var next_empty_delay: () => Int = () => {
    // with a delay of 3 cycles until state is shown, we can't have less
    // than 4 cycles of delay as a torture test, in practise we will have
    // more as this is done in software
    Random.nextInt(5) + 4
  }

  intf.full_n #= true
  var remainingSpace = 1
  var emptyDelay = 10
  val full = SimHistory(4, false)
  var packetBuffer = Queue[Int]()
  def fsm(): Unit = {
    val currentRemainingSpace = remainingSpace
    if (emptyDelay == 0) {
      remainingSpace = next_remaining_space()
      emptyDelay = next_empty_delay()
    }
    if (remainingSpace == 0) {
      emptyDelay = emptyDelay - 1
    }
    if (remainingSpace > 0 && !intf.wr_n.toBoolean) { // TODO should we check that we also indicate not-full?
      remainingSpace = remainingSpace - 1
      rxCallback(
        if (intf.dq.writeEnable.toBoolean) intf.dq.write.toInt else 0xffffffff
      )
      packetBuffer += intf.dq.write.toInt
    }
    if (currentRemainingSpace > 0 && (!intf.pktend_n.toBoolean || remainingSpace == 0)) {
      pktendCallback(packetBuffer)
      packetBuffer.clear()
      remainingSpace = 0
    }
    full.update(remainingSpace > 0)
    intf.full_n #= full(3)
  }
  clockDomain.onActiveEdges(fsm)
}
