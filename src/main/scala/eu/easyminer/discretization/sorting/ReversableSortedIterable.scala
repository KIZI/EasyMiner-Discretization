package eu.easyminer.discretization.sorting

import eu.easyminer.discretization

/**
  * Created by propan on 31. 3. 2017.
  */
trait ReversableSortedIterable[T] extends SortedIterable[T] {

  def reverse: SortedIterable[T]

}

object ReversableSortedIterable {

  private def reversedSortedIterable[T](it: discretization.ReversableSortedIterable[T])(implicit n: Ordering[T]): SortedIterable[T] = it.reverse()

  implicit def javaReversableSortedIterableToReversableSortedIterable[T](it: discretization.ReversableSortedIterable[T])(implicit n: Ordering[T]): ReversableSortedIterable[T] = new ReversableSortedIterable[T] {

    val sortedIterable: SortedIterable[T] = it

    def reverse: SortedIterable[T] = reversedSortedIterable(it)(n.reverse)

    def iterator: Iterator[T] = sortedIterable.iterator
  }

}