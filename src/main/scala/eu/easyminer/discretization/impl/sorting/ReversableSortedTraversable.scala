package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
class ReversableSortedTraversable[T](col: Traversable[T], colReversed: Traversable[T])(implicit n: Ordering[T]) extends SortedTraversable[T](col) {

  def reverse: SortedTraversable[T] = new SortedTraversable[T](colReversed)(n.reverse)

}

object ReversableSortedTraversable {

  implicit def javaReversableSortedIterableToReversableSortedTraversable[A <: Number, B](it: discretization.ReversableSortedIterable[A])(implicit n: Ordering[B], javaIteratorToIterator: java.util.Iterator[A] => Iterator[B]): ReversableSortedTraversable[B] = new ReversableSortedTraversable[B](
    new Traversable[B] {
      def foreach[U](f: B => U): Unit = it.iterator().foreach(f)
    },
    new Traversable[B] {
      def foreach[U](f: B => U): Unit = it.reverse().iterator().foreach(f)
    })

}