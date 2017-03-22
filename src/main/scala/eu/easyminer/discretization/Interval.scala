package eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 16. 3. 2017.
  */
case class Interval(minValue: IntervalBound, maxValue: IntervalBound)

object Interval {

  implicit def intervalToJavaInterval(interval: Interval): DefaultInterval = new DefaultInterval {
    def getLeftBoundValue: java.lang.Double = interval.minValue.value

    def getRightBoundValue: java.lang.Double = interval.maxValue.value

    def isLeftBoundClosed: java.lang.Boolean = interval.minValue.isInstanceOf[InclusiveIntervalBound]

    def isRightBoundClosed: java.lang.Boolean = interval.maxValue.isInstanceOf[InclusiveIntervalBound]

    def isLeftBoundOpened: java.lang.Boolean = !isLeftBoundClosed

    def isRightBoundOpened: java.lang.Boolean = !isRightBoundClosed
  }

}

sealed trait IntervalBound {
  val value: Double
}

case class InclusiveIntervalBound(value: Double) extends IntervalBound

case class ExclusiveIntervalBound(value: Double) extends IntervalBound

case class IntervalFrequency(interval: Interval, frequency: Int)

case class ValueFrequency[T](value: T, frequency: Int)(implicit n: Numeric[T])

object ValueFrequency {

  implicit def iterableToValueFrequencyIterable[T](it: Iterable[T])(implicit n: Numeric[T]): Iterable[ValueFrequency[T]] = new Iterable[ValueFrequency[T]] {
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