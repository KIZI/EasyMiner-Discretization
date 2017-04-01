package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
trait ReversableSortedIterable[T] extends SortedIterable[T] {

  def reverse: SortedIterable[T]

}

object ReversableSortedIterable {

  private def reversedSortedIterable[A <: Number, B](it: discretization.ReversableSortedIterable[A])(implicit n: Ordering[B], javaIteratorToIterator: java.util.Iterator[A] => Iterator[B]): SortedIterable[B] = it.reverse()

  implicit def javaReversableSortedIterableToReversableSortedIterable[A <: Number, B](it: discretization.ReversableSortedIterable[A])(implicit n: Ordering[B], javaIteratorToIterator: java.util.Iterator[A] => Iterator[B]): ReversableSortedIterable[B] = new ReversableSortedIterable[B] {

    val sortedIterable: SortedIterable[B] = it.asInstanceOf[discretization.SortedIterable[A]]

    def reverse: SortedIterable[B] = reversedSortedIterable(it)(n.reverse, javaIteratorToIterator)

    def iterator: Iterator[B] = sortedIterable.iterator
  }

}