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

    def asScala[B](implicit n: Numeric[B], numberToScalaNumber: A => B): Traversable[B] = {
      implicit val c: java.util.Iterator[A] => Iterator[B] = javaIteratorToIterator[A, B]
      it match {
        case it: ReversableSortedIterable[A] => it: sorting.ReversableSortedTraversable[B]
        case it: SortedIterable[A] => it: sorting.SortedTraversable[B]
        case _ => new Traversable[B] {
          def foreach[U](f: B => U): Unit = it.iterator().foreach(f)
        }
      }
    }

  }

}

object IterableConversions extends IterableConversions