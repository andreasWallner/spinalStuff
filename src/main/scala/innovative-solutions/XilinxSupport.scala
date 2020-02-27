package innovative_solutions

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{
  AxiLite4,
  AxiLite4Ax,
  AxiLite4W,
  AxiLite4B,
  AxiLite4R
}
import spinal.lib.bus.amba4.axi.{
  Axi4,
  Axi4Ax,
  Axi4W,
  Axi4B,
  Axi4R
}
import spinal.lib.IMasterSlave
import scala.collection.mutable.Map

/**
  * Rename Axi4-Lite bus as expected by Xilinx Vivado toolset
  *
  * according to https://www.xilinx.com/support/documentation/ip_documentation/axi_ref_guide/v13_4/ug761_axi_reference_guide.pdf
  */
object AxiLite4XilinxRenamer {
  def apply(axilite: AxiLite4, idx: Integer) = {
    val dirPrefix = if (axilite.isMasterInterface) "M" else "S"
    val idxPrefix = f"$idx%02d"
    val prefix = dirPrefix + idxPrefix + "_"

    renameAx(axilite.aw, prefix, "W")
    renameW(axilite.w, prefix)
    renameB(axilite.b, prefix)
    renameAx(axilite.ar, prefix, "R")
    renameR(axilite.r, prefix)
  }

  def renameAx(ax: Stream[AxiLite4Ax], prefix: String, rw: String) = {
    ax.addr.setName(prefix + f"A${rw}ADDR")
    ax.prot.setName(prefix + f"A${rw}PROT")
    ax.valid.setName(prefix + f"A${rw}VALID")
    ax.ready.setName(prefix + f"A${rw}READY")
  }

  def renameW(w: Stream[AxiLite4W], prefix: String) = {
    w.data.setName(prefix + f"WDATA")
    w.strb.setName(prefix + f"WSTRB")
    w.valid.setName(prefix + f"WVALID")
    w.ready.setName(prefix + f"WREADY")
  }

  def renameB(b: Stream[AxiLite4B], prefix: String) = {
    b.resp.setName(prefix + f"BRESP")
    b.valid.setName(prefix + f"BVALID")
    b.ready.setName(prefix + f"BREADY")
  }

  def renameR(r: Stream[AxiLite4R], prefix: String) = {
    r.data.setName(prefix + f"RDATA")
    r.resp.setName(prefix + f"RRESP")
    r.valid.setName(prefix + f"RVALID")
    r.ready.setName(prefix + f"RREADY")
  }
}

object Axi4XilinxRenamer {
  def apply(axi: Axi4, idx: Integer) = {
    val dirPrefix = if (axi.isMasterInterface) "M" else "S"
    val idxPrefix = f"$idx%02d"
    val prefix = dirPrefix + idxPrefix + "_"

    renameAx(axi.aw, prefix, "W")
    renameW(axi.w, prefix)
    renameB(axi.b, prefix)
    renameAx(axi.ar, prefix, "R")
    renameR(axi.r, prefix)
  }

  def renameAx[T <: Axi4Ax](ax: Stream[T], prefix: String, rw: String) = {
    ax.addr.setName(prefix + f"A${rw}ADDR")
    Option(ax.id) map(_.setName(prefix + f"A${rw}ID"))
    Option(ax.region) map(_.setName(prefix + f"A${rw}REGION"))
    Option(ax.len) map(_.setName(prefix + f"A${rw}LEN"))
    Option(ax.size) map(_.setName(prefix + f"A${rw}SIZE"))
    Option(ax.burst) map(_.setName(prefix + f"A${rw}BURST"))
    Option(ax.lock) map(_.setName(prefix + f"A${rw}LOCK"))
    Option(ax.cache) map(_.setName(prefix + f"A${rw}CACHE"))
    Option(ax.qos) map(_.setName(prefix + f"A${rw}QOS"))
    Option(ax.user) map(_.setName(prefix + f"A${rw}USER"))
    Option(ax.prot) map(_.setName(prefix + f"A${rw}PROT"))
    ax.valid.setName(prefix + f"A${rw}VALID")
    ax.ready.setName(prefix + f"A${rw}READY")
  }

  def renameW(w: Stream[Axi4W], prefix: String) = {
    w.data.setName(prefix + f"WDATA")
    Option(w.strb) map(_.setName(prefix + f"WSTRB"))
    Option(w.user) map(_.setName(prefix + f"WUSER"))
    Option(w.last) map(_.setName(prefix + f"WLAST"))
    w.valid.setName(prefix + f"WVALID")
    w.ready.setName(prefix + f"WREADY")
  }

  def renameB(b: Stream[Axi4B], prefix: String) = {
    Option(b.id) map(_.setName(prefix + f"BID"))
    Option(b.resp) map(_.setName(prefix + f"BRESP"))
    Option(b.user) map(_.setName(prefix + f"BUSER"))
    b.valid.setName(prefix + f"BVALID")
    b.ready.setName(prefix + f"BREADY")
  }

  def renameR(r: Stream[Axi4R], prefix: String) = {
    r.data.setName(prefix + f"RDATA")
    Option(r.id) map(_.setName(prefix + f"RID"))
    Option(r.resp) map(_.setName(prefix + f"RRESP"))
    Option(r.last) map(_.setName(prefix + f"RLAST"))
    Option(r.user) map(_.setName(prefix + f"RUSER"))
    r.valid.setName(prefix + f"RVALID")
    r.ready.setName(prefix + f"RREADY")
  }
}

object XilinxNamer {
  def name(comp: Component): Component = {
    var masterCnt = 0
    var slaveCnt = 0

    def doIt = {
      comp
        .getGroupedIO(true)
        .foreach((x) => {
          if (x.getName() == "clk") x.setName("ACLK")
          else if (x.getName() == "resetn") x.setName("ARESETN")
          else if (x.getName() == "reset") x.setName("ARESET")
          else {
            x match {
              case ms: IMasterSlave => {
                val cnt = if (ms.isMasterInterface) masterCnt else slaveCnt
                if (ms.isMasterInterface) masterCnt = masterCnt + 1
                else slaveCnt = slaveCnt + 1

                x match {
                  case axilite: AxiLite4 =>
                    AxiLite4XilinxRenamer(axilite, cnt)
                  case _ =>
                }
              }
              case _ =>
            }
          }
        })
    }
    comp.addPrePopTask(() => { doIt })
    comp
  }

  /*
  val ifCounts =
    Map[Class[_ <: Bundle], (Integer, Integer)]().withDefaultValue((0, 0))
  def getCnt(bundle: Bundle) = {
    val cnt = ifCounts(bundle.getClass)
    val (result, updated): (Integer, (Integer, Integer)) = bundle match {
      case ms: IMasterSlave =>
        ms.isMasterInterface match {
          case true  => (cnt._1, (cnt._1 + 1, cnt._2))
          case false => (cnt._2, (cnt._1, cnt._2 + 1))
        }
      case _ => (cnt._1, (cnt._1 + 1, 0))
    }
    ifCounts(bundle.getClass) = updated
    result
  }*/
}
