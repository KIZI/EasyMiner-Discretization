package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.impl.Producer
import eu.easyminer.discretization.impl.sorting.SortedInMemoryNumericProducer.Exceptions.BufferOverflowException

import scala.collection.mutable
import eu.easyminer.discretization.util.NumericByteArray._

/**
 * Created by propan on 30. 3. 2017.
 */
object SortedInMemoryNumericProducer {

  def apply[T](col: Producer[T], bufferSize: Int)(implicit n: Numeric[T]): ReversableSortedProducer[T] = {
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

    new ReversableSortedProducer(
      Producer(sortedCollection),
      Producer(new Iterable[T] {
        def iterator: Iterator[T] = sortedCollection.reverseIterator
      })
    )
  }

  object Exceptions {

    class BufferOverflowException(bytes: Int) extends Exception(s"Buffer overflow within SortedInMemoryNumericIterable: $bytes bytes")

  }

}