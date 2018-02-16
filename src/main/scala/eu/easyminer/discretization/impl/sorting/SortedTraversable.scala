package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 18. 3. 2017.
  */
class SortedTraversable[T](col: Traversable[T])(implicit n: Ordering[T]) extends Traversable[T] {

  def foreach[U](f: T => U): Unit = {
    var prev = Option.empty[T]
    for (value <- col) {
      if (prev.forall(x => n.lteq(x, value))) {
        prev = Some(value)
        f(value)
      } else {
        throw new IllegalStateException()
      }
    }
  }

}

object SortedTraversable {

  implicit def javaSortedIterableToSortedTraversable[A <: Number, B](it: discretization.SortedIterable[A])(implicit n: Ordering[B], javaIteratorToIterator: java.util.Iterator[A] => Iterator[B]): SortedTraversable[B] = new SortedTraversable[B](new Traversable[B] {
    def foreach[U](f: B => U): Unit = it.iterator().foreach(f)
  })

}