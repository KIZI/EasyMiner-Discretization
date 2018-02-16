package eu.easyminer.discretization.impl

import eu.easyminer.discretization.impl.sorting.SortedTraversable

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
case class ValueFrequency[T](value: T, frequency: Int)

object ValueFrequency {

  implicit def sortedIterableToValueFrequencyIterable[T](it: SortedTraversable[T])(implicit n: Numeric[T]): Iterable[ValueFrequency[T]] = new Iterable[ValueFrequency[T]] {
    def iterator: Iterator[ValueFrequency[T]] = new Iterator[ValueFrequency[T]] {
      val _it = it.iterator
      var lastValue = Option.empty[T]

      @scala.annotation.tailrec
      def loadValueFrequency(v: ValueFrequency[T]): ValueFrequency[T] = if (_it.hasNext) {
        val x = _it.next()
        if (n.equiv(x, v.value)) {
          loadValueFrequency(v.copy(frequency = v.frequency + 1))
        } else {
          lastValue = Some(x)
          v
        }
      } else {
        lastValue = None
        v
      }

      def hasNext: Boolean = _it.hasNext || lastValue.nonEmpty

      def next(): ValueFrequency[T] = loadValueFrequency(ValueFrequency(lastValue.getOrElse(_it.next()), 1))
    }
  }

}