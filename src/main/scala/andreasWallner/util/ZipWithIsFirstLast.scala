package andreasWallner.util

import scala.collection.{AbstractIterator, AbstractView}
import scala.collection.View.SomeIterableOps

class ZipWithIsFirstLast[A](underlying: SomeIterableOps[A])
    extends AbstractView[(A, Boolean, Boolean)] {
  def makeIterator(source: Iterator[A]) = new AbstractIterator[(A, Boolean, Boolean)] {
    var first = true

    def hasNext = source.hasNext

    def next(): (A, Boolean, Boolean) = {
      val current = source.next()
      val currentFirst = first
      first = false
      (current, currentFirst, !source.hasNext)
    }
  }

  def iterator: Iterator[(A, Boolean, Boolean)] = makeIterator(underlying.iterator)

  override def knownSize: Int = underlying.knownSize

  override def isEmpty: Boolean = underlying.isEmpty
}
