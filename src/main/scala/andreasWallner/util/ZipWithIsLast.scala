package andreasWallner.util

import scala.collection.{AbstractIterator, AbstractView}
import scala.collection.View.SomeIterableOps

class ZipWithIsLast[A](underlying: SomeIterableOps[A]) extends AbstractView[(A, Boolean)] {
  def makeIterator(source: Iterator[A]) = new AbstractIterator[(A, Boolean)] {
    def hasNext = source.hasNext

    def next(): (A, Boolean) = {
      val current = source.next()
      (current, !source.hasNext)
    }
  }

  def iterator: Iterator[(A, Boolean)] = makeIterator(underlying.iterator)

  override def knownSize: Int = underlying.knownSize

  override def isEmpty: Boolean = underlying.isEmpty
}
