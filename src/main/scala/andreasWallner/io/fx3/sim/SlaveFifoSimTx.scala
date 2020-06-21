package andreasWallner.io.fx3.sim

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import andreasWallner.io.fx3._

case class SlaveFifoSimTx(intf: SlaveFifo, clockDomain: ClockDomain)(
    txCallback: () => (Boolean, Int)
) {
  var next_block_delay: () => Int = () => {
    Random.nextInt(10) + 2
  }
  var next_block_size: () => Int = () => {
    Random.nextInt(10)
  }

  // txCallback -> data to buffer if empty, then use this 
  intf.empty_n #= false
  var remaining_block_size = 0
  var remaining_block_delay = 0
  var buffer_valid = false
  var buffer = 0
  var empty_n_next = false
  var last_rd_n = true
  def fsm(): Unit = {
    if (remaining_block_delay == 0 && remaining_block_size == 0) {
      remaining_block_delay = next_block_delay()
      remaining_block_size = next_block_size()
    }
    if (remaining_block_size == 0 && remaining_block_delay > 0) {
      remaining_block_delay = remaining_block_delay - 1
    }
    if (!buffer_valid) {
      val (valid, x) = txCallback()
      buffer_valid = valid
      buffer = x
    }
    intf.empty_n #= empty_n_next
    if ((intf.empty_n.toBoolean && !last_rd_n && remaining_block_size > 0)) {
      remaining_block_size = remaining_block_size - 1

      intf.dq.read #= buffer
      
      val (valid, x) = txCallback()
      buffer_valid = valid
      buffer = x
      empty_n_next = buffer_valid && (remaining_block_size > 0)
    } else {
      empty_n_next = buffer_valid && (remaining_block_size > 0)
    }
    last_rd_n = intf.rd_n.toBoolean
  }
  clockDomain.onActiveEdges(fsm)
}