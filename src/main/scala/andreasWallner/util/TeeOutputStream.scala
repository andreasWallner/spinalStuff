package andreasWallner.util

import java.io.{FilterOutputStream, OutputStream}

/**
  * Output stream that write output to two underlying streams (like the Unix `tee` utility)
  *
  * Heavily inspired by the Apache common-io TeeOutputStream (but w/o the customization ability of `ProxyOutputStream`)
  * https://github.com/apache/commons-io/blob/b51e41938ea794f67223c1414c9e6de8a04c17b5/src/main/java/org/apache/commons/io/output/TeeOutputStream.java
  */
class TeeOutputStream(out: OutputStream, branch: OutputStream) extends FilterOutputStream(out) {
  override def close(): Unit = {
    try {
      super.close()
    } finally {
      branch.close()
    }
  }

  override def flush(): Unit = {
    super.flush()
    branch.flush()
  }

  override def write(b: Array[Byte]): Unit = {
    super.write(b)
    branch.write(b)
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    super.write(b, off, len)
    branch.write(b, off, len)
  }
}
