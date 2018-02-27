package eu.easyminer.discretization.impl

import eu.easyminer.discretization.impl.sorting.SortedTraversable

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
case class ValueFrequency[T](value: T, frequency: Int)

object ValueFrequency {

  implicit def sortedTraversableToValueFrequencyTraversable[T](it: SortedTraversable[T])(implicit n: Numeric[T]): Traversable[ValueFrequency[T]] = new Traversable[ValueFrequency[T]] {
    def foreach[U](f: ValueFrequency[T] => U): Unit = {
      var lastValue: Option[ValueFrequency[T]] = None
      for (value <- it) {
        lastValue match {
          case Some(x) if n.equiv(x.value, value) => lastValue = Some(x.copy(frequency = x.frequency + 1))
          case _ =>
            lastValue.foreach(f)
            lastValue = Some(ValueFrequency(value, 1))
        }
      }
      lastValue.foreach(f)
    }
  }

}