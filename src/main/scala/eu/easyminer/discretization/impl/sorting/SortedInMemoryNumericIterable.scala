package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.impl.sorting.SortedInMemoryNumericIterable.Exceptions.BufferOverflowException

import scala.collection.mutable
import eu.easyminer.discretization.util.NumericByteArray._

/**
  * Created by propan on 30. 3. 2017.
  */
class SortedInMemoryNumericIterable[T] private(it: Iterator[T], bufferSize: Int)(implicit n: Numeric[T]) extends Iterable[T] with ReversableSortedIterable[T] {

  private lazy val sortedCollection = {
    val numberOfValues = math.floor(bufferSize.toDouble / n.zero.length)
    val sortedCollection = mutable.PriorityQueue.empty[T](n.reverse)
    val (it1, it2) = it.zipWithIndex.span(_._2 < numberOfValues)
    sortedCollection ++= it1.map(_._1)
    if (it2.hasNext) {
      throw new BufferOverflowException(bufferSize)
    }
    sortedCollection.dequeueAll
  }

  def iterator: Iterator[T] = sortedCollection.iterator

  def reverse: SortedIterable[T] = new SortedIterable[T] {
    def iterator: Iterator[T] = sortedCollection.reverseIterator
  }

}

object SortedInMemoryNumericIterable {

  def apply[T](it: Iterator[T], bufferSize: Int)(implicit n: Numeric[T]): ReversableSortedIterable[T] = new SortedInMemoryNumericIterable(it, bufferSize)

  object Exceptions {

    class BufferOverflowException(bytes: Int) extends Exception(s"Buffer overflow within SortedInMemoryNumericIterable: $bytes bytes")

  }

}