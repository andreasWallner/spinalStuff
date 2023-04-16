package andreasWallner.util

object Timer {
  // thanks to Andrew Zakordonets (https://biercoff.com/easily-measuring-code-execution-time-in-scala/)
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + " ns")
    result
  }

  def time[R](id: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(id + " elapsed time: " + (t1 - t0) + " ns")
    result
  }
}
