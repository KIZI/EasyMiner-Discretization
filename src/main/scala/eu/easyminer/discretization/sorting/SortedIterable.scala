package eu.easyminer.discretization.sorting

/**
  * Created by propan on 18. 3. 2017.
  */
class SortedIterable[T](iterable: Iterable[T])(implicit o: Ordering[T]) extends Iterable[T] {

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
      if (prev.forall(x => o.lteq(x, value))) {
        prev = Some(value)
        value
      } else {
        throw new IllegalStateException()
      }
    }
  }

  def iterator: Iterator[T] = if (sorted) iterable.iterator else desiredSortedIterator(iterable.iterator)

}

object SortedIterable {

  implicit def iterableToSortedIterable[T](it: Iterable[T])(implicit n: Ordering[T]): SortedIterable[T] = new SortedIterable[T](it)

}

case class SortedIterables[T](asc: SortedIterable[T], desc: SortedIterable[T])