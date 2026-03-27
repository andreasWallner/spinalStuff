/*package andreasWallner

import spinal.core._
import spinal.lib._
import andreasWallner.bus.amba3.ahblite3._
import andreasWallner.io.spi.SpiSlavePeripheral
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.misc.SizeMapping
import andreasWallner.bus.amba3.ahblite3.AhbLite3SlaveFactory
import spinal.core.internals.Statement

import scala.collection.mutable
import scala.language.postfixOps

object MakeExample extends App {
  SpinalVerilog(
    AhbLite3Interconnect(
      2,
      2,
      Seq(
        SizeMapping(0x0000, 0x4000),
        SizeMapping(0x8000, 0x100)
      ),
      ahbConfig = AhbLite3Config(addressWidth = 32, dataWidth = 32)
    )
  )
}

object MakeInterconnect extends App {
  SpinalConfig(defaultConfigForClockDomains =
    ClockDomainConfig(resetActiveLevel = LOW, resetKind = ASYNC, softResetActiveLevel = HIGH)
  ).generateVerilog {
    val c = ClockDomain.external("myclock", withReset=true, withSoftReset=true) {
      AhbLite3Interconnect(
        4,
        12,
        Seq(
          SizeMapping(0x0000, 0x4000),
          SizeMapping(0x8000, 0x100),
          SizeMapping(0x8100, 0x100),
          SizeMapping(0x8200, 0x100),
          SizeMapping(0x8300, 0x100),
          SizeMapping(0x8400, 0x100),
          SizeMapping(0x8500, 0x100),
          SizeMapping(0x8600, 0x100),
          SizeMapping(0x8700, 0x100),
          SizeMapping(0x9000, 0x1000),
          SizeMapping(0x10000, 0xa000),
          SizeMapping(0x20000, 0x20000)
        ),
        ahbConfig = AhbLite3Config(addressWidth = 32, dataWidth = 32)
      )
    }
    c.setDefinitionName("TopSoC_cli_riscv_Bus")

    def nameM(ahb: AhbLite3Master, newPrefix: String): Unit = {
      ahb.flatten.foreach(bt => bt.setName(newPrefix + "_" + bt.getName().split("_").last))
    }

    nameM(c.io.masters(0), "Bus_InstructionBus_Debug")
    nameM(c.io.masters(1), "Bus_DataBus_DebugInterface")
    nameM(c.io.masters(2), "Bus_MasterInterface_0")
    nameM(c.io.masters(3), "Bus_MasterInterface_1")

    def name(ahb: AhbLite3, newPrefix: String): Unit =
      ahb.flatten.foreach(bt => bt.setName(newPrefix + "_" + bt.getName().split("_").last))

    name(c.io.slaves(0), "Bus_BootROM_Interface")
    name(c.io.slaves(1), "Bus_Timer_Interface")
    name(c.io.slaves(2), "Bus_PIC_Interface")
    name(c.io.slaves(3), "Bus_SPI_Interface")
    name(c.io.slaves(4), "Bus_UART_Interface")
    name(c.io.slaves(5), "Bus_IOPad_Interface")
    name(c.io.slaves(6), "Bus_Radio_Interface")
    name(c.io.slaves(7), "Bus_ConnDev_Interface")
    name(c.io.slaves(8), "Bus_ADC_Interface")
    name(c.io.slaves(9), "Bus_Scope_Interface")
    name(c.io.slaves(10), "Bus_DataMemory_Interface")
    name(c.io.slaves(11), "Bus_InstructionMemory_Interface")

    c.rework {
      ClockDomain.current.renamePulledWires("clk", "reset_n_i", "debug_ndmrst")

      def shrinkAddr(addr: UInt, width: BitCount) = {
        val shrunk = UInt(width).setName(addr.getName())
        shrunk.copyDirectionOf(addr)
        shrunk := addr.resized
        addr.setAsDirectionLess().unsetName().allowDirectionLessIo()
      }
      shrinkAddr(c.io.slaves(0).HADDR, 14 bit)
      shrinkAddr(c.io.slaves(1).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(2).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(3).HADDR, 4 bit)
      shrinkAddr(c.io.slaves(4).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(5).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(6).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(7).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(8).HADDR, 8 bit)
      shrinkAddr(c.io.slaves(9).HADDR, 10 bit)
      shrinkAddr(c.io.slaves(10).HADDR, 16 bit)
      shrinkAddr(c.io.slaves(11).HADDR, 17 bit)

      c.noIoPrefix()
      c.io.masters.foreach { m =>
        m.HBURST := 0
        m.HBURST.setAsDirectionLess().unsetName().allowDirectionLessIo()
        m.HPROT := 0
        m.HPROT.setAsDirectionLess().unsetName().allowDirectionLessIo()
        m.HMASTLOCK := False
        m.HMASTLOCK.setAsDirectionLess().unsetName().allowDirectionLessIo()
      }
      c.io.slaves.foreach { s =>
        s.HBURST.setAsDirectionLess().unsetName().allowDirectionLessIo()
        s.HPROT.setAsDirectionLess().unsetName().allowDirectionLessIo()
        s.HMASTLOCK.setAsDirectionLess().unsetName().allowDirectionLessIo()
        //s.HREADY.setAsDirectionLess().unsetName().allowDirectionLessIo()
        s.HREADY.setName(
          s.HREADY.getName().replace("HREADY", "HREADYIN")
        )
        s.HREADYOUT.setName(
          s.HREADYOUT.getName().replace("HREADYOUT", "HREADY")
        )
      }
      c
    }
  }
}

class ipdb_common_sync extends BlackBox {
  val clk = in port Bool()
  val reset_n_i = in port Bool()
  val data_i = in port Bool()
  val data_o = out port Bool()

  mapCurrentClockDomain(clk, reset_n_i)
}

object MakeSPI extends App {
  val ahbConfig = AhbLite3Config(4, 32)
  SpinalConfig(defaultConfigForClockDomains =
    ClockDomainConfig(resetActiveLevel = LOW, resetKind = ASYNC, softResetActiveLevel = HIGH),
  ).generateVerilog {
    val c = ClockDomain.external("myclock", withReset=true, withSoftReset=true) {
      val c = SpiSlavePeripheral[AhbLite3](AhbLite3(ahbConfig), new AhbLite3SlaveFactory(_))
      c
    }

    c.rework {
      ClockDomain.current.renamePulledWires("clk", "reset_n_i", "debug_ndmrst")

      val cs_sync = c.core.sync.cs.pull()
      val highz = Bool()
      highz.asOutput().setName("HiZ_spiModule")
      highz := cs_sync
      c.io.spi.miso.writeEnable.setAsDirectionLess().unsetName().allowDirectionLessIo()

      c.io.spi.cs.setName("CS_spiModule")
      c.io.spi.mosi.setName("serial_in_spiModule")
      c.io.spi.miso.write.setName("serial_out_spiModule")
      c.io.spi.sclk.setName("SCK_spiModule")

      c.io.bus.setName("spi")

      c.io.bus.HPROT.clearAll()
      c.io.bus.HPROT.setAsDirectionLess().unsetName().allowDirectionLessIo()
      c.io.bus.HBURST.clearAll()
      c.io.bus.HBURST.setAsDirectionLess().unsetName().allowDirectionLessIo()
      c.io.bus.HMASTLOCK := False
      c.io.bus.HMASTLOCK.setAsDirectionLess().unsetName().allowDirectionLessIo()
      c.io.bus.HREADY.setName(
        c.io.bus.HREADY.getName().replace("HREADY", "HREADYIN")
      )
      c.io.bus.HREADY := c.io.bus.HREADYOUT
      c.io.bus.HREADY.setAsDirectionLess().unsetName().allowDirectionLessIo()
      c.io.bus.HREADYOUT.setName(
        c.io.bus.HREADYOUT.getName().replace("HREADYOUT", "HREADY")
      )
    }
/*
    c.walkComponents {
      case bcc: BufferCC[_] =>
        bcc.rework {
          val statements = mutable.ArrayBuffer[Statement]()
          bcc.dslBody.walkStatements {
            case bt: BaseType if(!bt.isDirectionLess) => // keep
            case s => statements += s
        }
        statements.foreach(_.removeStatement())
        val sync = new ipdb_common_sync()
        // TODO !!!!
      case _ =>
    }
*/
    def resetNonReset(comp: Component): Unit = {
      c.rework {
        c.dslBody.walkStatements { s =>
          s.walkExpression {
            case d: Bits =>
              if (d.isReg && !d.hasInit) {
                d.init(B(0, d.getBitsWidth bit))
              }
            case b: Bool =>
              if (b.isReg && !b.hasInit) {
                b.init(False)
              }
            // TODO the other types, or find a shortcut way to initialize all BitVector types
            case _ =>
          }
        }
      }
    }
    c.walkComponents(resetNonReset)
    c
  }
}

object Blub extends App {
  SpinalVerilog(new Component {
    val i = in port Bool()
    val o = out port Bool()
    o := BufferCC(i, randBoot=true)
  })
}
*/
