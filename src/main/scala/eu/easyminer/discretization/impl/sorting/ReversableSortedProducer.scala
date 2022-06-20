package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.impl.Producer

import scala.language.implicitConversions

/**
 * Created by propan on 31. 3. 2017.
 */
class ReversableSortedProducer[T](col: Producer[T], colReversed: Producer[T])(implicit n: Ordering[T]) extends SortedProducer[T](col) {
  def reverse: SortedProducer[T] = new SortedProducer[T](colReversed)(n.reverse)
}

object ReversableSortedProducer {

  implicit def javaReversableSortedProducerToReversableSortedProducer[A <: Number, B](it: eu.easyminer.discretization.ReversableSortedProducer[A])(implicit n: Ordering[B], numberToScalaNumber: A => B): ReversableSortedProducer[B] = new ReversableSortedProducer[B](
    Producer(it).map(numberToScalaNumber),
    Producer(it.reverse()).map(numberToScalaNumber)
  )

}