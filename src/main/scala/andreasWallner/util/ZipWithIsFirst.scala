package andreasWallner.util

import scala.collection.{AbstractIterator, AbstractView}
import scala.collection.View.SomeIterableOps

class ZipWithIsFirst[A](underlying: SomeIterableOps[A]) extends AbstractView[(A, Boolean)] {
  def makeIterator(source: Iterator[A]) = new AbstractIterator[(A, Boolean)] {
    var first = true

    def hasNext = source.hasNext

    def next(): (A, Boolean) = {
      val current = first
      first = false
      (source.next(), current)
    }
  }

  def iterator: Iterator[(A, Boolean)] = makeIterator(underlying.iterator)

  override def knownSize: Int = underlying.knownSize

  override def isEmpty: Boolean = underlying.isEmpty
}
