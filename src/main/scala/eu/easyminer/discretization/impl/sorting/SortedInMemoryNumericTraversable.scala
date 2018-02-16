package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.impl.sorting.SortedInMemoryNumericTraversable.Exceptions.BufferOverflowException

import scala.collection.mutable
import eu.easyminer.discretization.util.NumericByteArray._

/**
  * Created by propan on 30. 3. 2017.
  */
object SortedInMemoryNumericTraversable {

  def apply[T](col: Traversable[T], bufferSize: Int)(implicit n: Numeric[T]): ReversableSortedTraversable[T] = {
    lazy val sortedCollection = {
      val numberOfValues = math.floor(bufferSize.toDouble / n.zero.length)
      val sortedCollection = mutable.PriorityQueue.empty[T](n.reverse)
      var i = 0
      for (value <- col) {
        if (i >= numberOfValues) throw new BufferOverflowException(bufferSize)
        sortedCollection enqueue value
        i += 1
      }
      sortedCollection.dequeueAll
    }

    new ReversableSortedTraversable(
      new Traversable[T] {
        def foreach[U](f: T => U): Unit = sortedCollection.foreach(f)
      },
      new Traversable[T] {
        def foreach[U](f: T => U): Unit = sortedCollection.reverseIterator.foreach(f)
      }
    )
  }

  object Exceptions {

    class BufferOverflowException(bytes: Int) extends Exception(s"Buffer overflow within SortedInMemoryNumericIterable: $bytes bytes")

  }

}