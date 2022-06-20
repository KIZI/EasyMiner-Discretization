package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.Consumer
import eu.easyminer.discretization.impl.Producer

import scala.language.implicitConversions

/**
 * Created by propan on 18. 3. 2017.
 */
class SortedProducer[T](col: Producer[T])(implicit n: Ordering[T]) extends Producer[T] {

  def produce(consumer: Consumer[T]): Unit = {
    var prev = Option.empty[T]
    for (value <- col) {
      if (prev.forall(x => n.lteq(x, value))) {
        prev = Some(value)
        consumer.consume(value)
      } else {
        throw new IllegalStateException()
      }
    }
  }

}

object SortedProducer {

  implicit def javaSortedProducerToSortedProducer[A <: Number, B](it: eu.easyminer.discretization.SortedProducer[A])(implicit n: Ordering[B], numberToScalaNumber: A => B): SortedProducer[B] = new SortedProducer(Producer(it).map(numberToScalaNumber))

}