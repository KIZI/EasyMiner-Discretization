package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 18. 3. 2017.
  */
trait SortedIterable[T] extends Iterable[T]

object SortedIterable {

  implicit def javaSortedIterableToSortedIterable[A <: Number, B](it: discretization.SortedIterable[A])(implicit n: Ordering[B], javaIteratorToIterator: java.util.Iterator[A] => Iterator[B]): SortedIterable[B] = new SortedIterable[B] {

    private var sorted = false

    private def desiredSortedIterator(it: Iterator[B]): Iterator[B] = new Iterator[B] {
      private var prev = Option.empty[B]

      def hasNext: Boolean = {
        val value = it.hasNext
        if (!value) sorted = true
        value
      }

      def next(): B = {
        val value = it.next()
        if (prev.forall(x => n.lteq(x, value))) {
          prev = Some(value)
          value
        } else {
          throw new IllegalStateException()
        }
      }
    }

    def iterator: Iterator[B] = if (sorted) it.iterator() else desiredSortedIterator(it.iterator())

  }

}