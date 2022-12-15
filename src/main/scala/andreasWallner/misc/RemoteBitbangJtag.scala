package andreasWallner.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib.com.jtag.Jtag

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

case class RemoteBitbangJtag(jtag: Jtag, cd: ClockDomain, trst: Option[Bool] = None, srst: Option[Bool] = None, port: Int = 3335) {
  val selector = Selector.open()
  val serverSocketChannel = ServerSocketChannel.open()
  serverSocketChannel.configureBlocking(false)
  serverSocketChannel.bind(new InetSocketAddress("0.0.0.0", port))
  serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT)
  val bb = ByteBuffer.allocate(1)

  jtag.tck #= false
  jtag.tms #= true
  jtag.tdi #= true
  // do not initially drive trst & srst to let the user choose their default values

  forkSensitive {
    if (selector.select() > 0) {
      val selectedKeys = selector.selectedKeys()
      val iterator = selectedKeys.iterator()
      while (iterator.hasNext) {
        val key = iterator.next()
        if (key.isAcceptable) {
          val sc = serverSocketChannel.accept()
          sc.configureBlocking(false)
          sc.register(selector, SelectionKey.OP_READ)
        }
        if (key.isReadable) {
          val sc = key.channel().asInstanceOf[SocketChannel]
          sc.read(bb)
          if (bb.array().length <= 0) {
            sc.close()
          } else {
            processCmd(bb.array()(0), sc)
          }
        }
      }
    }
  }

  val ascii1: Byte = '1'
  val ascii0: Byte = '0'

  def processCmd(cmd: Byte, sc: SocketChannel): Unit = {
    cmd match {
      case 'B' | 'b' => // ignore blink for now
      case 'R' =>
        val resp = ByteBuffer.allocate(1)
        resp.array()(0) = if (jtag.tms.toBoolean) ascii1 else ascii0
        sc.write(resp)
      case w if w >= '0' && w <= '7' =>
        val bits = w - '0'
        jtag.tdi #= (bits & 1) != 0
        jtag.tms #= (bits & 2) != 0
        jtag.tck #= (bits & 3) != 0
      case 'r' =>
        trst.foreach(_ #= false)
        srst.foreach(_ #= false)
      case 's' =>
        trst.foreach(_ #= false)
        srst.foreach(_ #= true)
      case 't' =>
        trst.foreach(_ #= true)
        srst.foreach(_ #= false)
      case 'u' =>
        trst.foreach(_ #= true)
        srst.foreach(_ #= true)
      case 'Q' | _ => sc.close()
    }
  }
}
