package eu.easyminer.discretization.impl

import eu.easyminer.discretization.{ReversableSortedIterable, SortedIterable}

import scala.language.implicitConversions

/**
  * Created by propan on 1. 4. 2017.
  */
trait IterableConversions {

  implicit def javaIteratorToIterator[A <: Number, B](it: java.util.Iterator[A])(implicit n: Numeric[B], numberToScalaNumber: A => B): Iterator[B] = new Iterator[B] {
    def hasNext: Boolean = it.hasNext

    def next(): B = it.next()
  }

  implicit class PimpedJavaIterable[A <: Number](it: java.lang.Iterable[A]) {

    def asScala[B](implicit n: Numeric[B], numberToScalaNumber: A => B): Iterable[B] = {
      implicit val c: java.util.Iterator[A] => Iterator[B] = javaIteratorToIterator[A, B] _
      it match {
        case it: ReversableSortedIterable[A] => it: sorting.ReversableSortedIterable[B]
        case it: SortedIterable[A] => it: sorting.SortedIterable[B]
        case _ => new Iterable[B] {
          def iterator: Iterator[B] = it.iterator()
        }
      }
    }

  }

}

object IterableConversions extends IterableConversions