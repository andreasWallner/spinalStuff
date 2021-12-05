package andreasWallner.audio.io.i2s

import spinal.core._
import spinal.sim._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim._
import andreasWallner.sim.simLog
import org.scalatest.FunSuite

case class I2STester(in_width: Int, ctrl_width: Int, out_width: Int)
    extends Component {
  val io = new Bundle {
    val rx = new Bundle {
      val left = master(Flow(UInt(out_width bit)))
      val right = master(Flow(UInt(out_width bit)))
    }
    val tx = new Bundle {
      val left = slave(Stream(UInt(in_width bit)))
      val right = slave(Stream(UInt(in_width bit)))
    }
    val i2s = new Bundle {
      val sd = out Bool ()
      val ws = out Bool ()
      val sck = out Bool ()
    }
  }

  val rx = I2SReceiver(out_width)
  val tx = I2STransmitter(in_width)
  val ctrl = I2SController(5, ctrl_width)

  rx.io.i2s.sd := tx.io.i2s.sd
  rx.io.i2s.ws := ctrl.io.i2s.ws
  rx.io.i2s.sck := ctrl.io.i2s.sck
  io.rx.left <> rx.io.audio.left
  io.rx.right <> rx.io.audio.right

  tx.io.i2s.ws := ctrl.io.i2s.ws
  tx.io.i2s.sck := ctrl.io.i2s.sck
  io.tx.left <> tx.io.audio.left
  io.tx.right <> tx.io.audio.right

  io.i2s.sd := tx.io.i2s.sd
  io.i2s.ws := ctrl.io.i2s.ws
  io.i2s.sck := ctrl.io.i2s.sck
}

case class I2STest() extends FunSuite {
  def shift(value: Int, shift: Int) =
    if (shift > 0) (value >> shift) else (value << -shift)

  for ((in_width, ctrl_width, out_width) <- List(
         (24, 24, 24),
         (10, 10, 10),
         (5, 10, 10),
         (10, 5, 10),
         (10, 10, 5),
         (5, 5, 10),
         (5, 10, 5),
         (10, 5, 5),
         (5, 5, 5)
       )) {
    val name = f"I2STest_${in_width}_${ctrl_width}_${out_width}"

    test(name) {
      def calcResult(value: Int) = {
        val transmitted = shift(value, in_width - ctrl_width)
        val received = shift(transmitted, ctrl_width - out_width)
        received
      }

      val dut =
        SimConfig.withWave.compile(I2STester(in_width, ctrl_width, out_width))

      dut.doSim(name) { dut =>
        val toSend = 10
        SimTimeout(toSend * 4 * 10 * 10 * ctrl_width)

        val left_scoreboard = ScoreboardInOrder[Int]()
        val right_scoreboard = ScoreboardInOrder[Int]()

        dut.io.tx.left.valid #= true
        dut.io.tx.right.valid #= true
        dut.io.tx.left.payload.randomize
        dut.io.tx.right.payload.randomize

        dut.clockDomain.forkStimulus(10)

        dut.clockDomain.waitActiveEdgeWhere(dut.io.i2s.ws.toBoolean == true)
        dut.clockDomain.waitActiveEdgeWhere(dut.io.i2s.ws.toBoolean == false)
        // we are synced now since the output will be kept driven until the next sck falling edge

        StreamDriver(dut.io.tx.left, dut.clockDomain) { payload =>
          payload.randomize
          true
        }.transactionDelay = () => 0
        StreamDriver(dut.io.tx.right, dut.clockDomain) { payload =>
          payload.randomize
          true
        }.transactionDelay = () => 0
        StreamMonitor(dut.io.tx.left, dut.clockDomain) { payload =>
          left_scoreboard.pushRef(calcResult(payload.toInt))
        }
        StreamMonitor(dut.io.tx.right, dut.clockDomain) { payload =>
          right_scoreboard.pushRef(calcResult(payload.toInt))
        }

        // wait for the last reception from the data before the sync
        dut.clockDomain.waitActiveEdgeWhere(
          dut.io.rx.right.valid.toBoolean == true
        )

        FlowMonitor(dut.io.rx.left, dut.clockDomain) { payload =>
          left_scoreboard.pushDut(payload.toInt)
        }
        FlowMonitor(dut.io.rx.right, dut.clockDomain) { payload =>
          right_scoreboard.pushDut(payload.toInt)
        }

        dut.clockDomain.waitActiveEdgeWhere(
          left_scoreboard.matches > toSend && right_scoreboard.matches > toSend
        )
      }
    }
  }
}
