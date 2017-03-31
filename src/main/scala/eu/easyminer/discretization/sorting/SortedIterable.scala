package eu.easyminer.discretization.sorting

import eu.easyminer.discretization

/**
  * Created by propan on 18. 3. 2017.
  */
trait SortedIterable[T] extends Iterable[T]

object SortedIterable {

  import scala.collection.JavaConversions._

  implicit def javaSortedIterableToSortedIterable[T](it: discretization.SortedIterable[T])(implicit n: Ordering[T]): SortedIterable[T] = new SortedIterable[T] {

    private var sorted = false

    private def desiredSortedIterator(it: Iterator[T]): Iterator[T] = new Iterator[T] {
      private var prev = Option.empty[T]

      def hasNext: Boolean = {
        val value = it.hasNext
        if (!value) sorted = true
        value
      }

      def next(): T = {
        val value = it.next()
        if (prev.forall(x => n.lteq(x, value))) {
          prev = Some(value)
          value
        } else {
          throw new IllegalStateException()
        }
      }
    }

    def iterator: Iterator[T] = if (sorted) it.iterator() else desiredSortedIterator(it.iterator())

  }

}