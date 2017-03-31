package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.Discretization.Exceptions.IllegalTypeOfIterable
import eu.easyminer.discretization.interval.{InclusiveIntervalBound, Interval, IntervalFrequency, ValueFrequency}
import eu.easyminer.discretization.sorting.ReversableSortedIterable
import eu.easyminer.discretization.{AbsoluteSupport, Discretization, RelativeSupport, Support}
import IntervalSmoothing._
import CutpointsResolver._

/**
  * Created by propan on 31. 3. 2017.
  */
class EquisizedIntervals[T](minSupport: Support)(implicit val n: Numeric[T]) extends Discretization[T] {

  private def countOptimalFrequency(data: Iterable[T]) = minSupport match {
    case RelativeSupport(minSupport) => math.ceil(data.iterator.size * minSupport).toInt
    case AbsoluteSupport(minSupport) => minSupport
  }

  private def searchIntervals(data: Iterable[ValueFrequency[T]], optimalFrequency: Int) = {
    val intervals = new collection.mutable.ArrayBuffer[IntervalFrequency]()
    for (value <- data.iterator) {
      intervals
        .lastOption
        .filter(interval => interval.frequency < optimalFrequency) match {
        case Some(interval) => intervals.update(intervals.length - 1, IntervalFrequency(interval.interval.copy(maxValue = InclusiveIntervalBound(n.toDouble(value.value))), interval.frequency + value.frequency))
        case None =>
          val leftRightBound = InclusiveIntervalBound(n.toDouble(value.value))
          intervals += IntervalFrequency(Interval(leftRightBound, leftRightBound), value.frequency)
      }
    }
    if (intervals.length > 1) {
      intervals.lastOption.filter(_.frequency < optimalFrequency) foreach { lastInterval =>
        val prevLastInterval = intervals(intervals.length - 2)
        intervals.update(intervals.length - 2, IntervalFrequency(Interval(prevLastInterval.interval.minValue, lastInterval.interval.maxValue), lastInterval.frequency + prevLastInterval.frequency))
        intervals.reduceToSize(intervals.length - 1)
      }
    }
    intervals
  }

  private def canItMoveLeft(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: IntervalFrequency, rightInterval: IntervalFrequency) = {
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val decreasedIntervalFreqency = rightInterval.frequency - movedValue.frequency
    val nextDifference = math.abs(decreasedIntervalFreqency - (leftInterval.frequency + movedValue.frequency))
    decreasedIntervalFreqency >= optimalFrequency && nextDifference < currentDifference
  }

  private def canItMoveRight(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: IntervalFrequency, rightInterval: IntervalFrequency) = {
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val decreasedIntervalFreqency = leftInterval.frequency - movedValue.frequency
    val nextDifference = math.abs((rightInterval.frequency + movedValue.frequency) - decreasedIntervalFreqency)
    decreasedIntervalFreqency >= optimalFrequency && nextDifference < currentDifference
  }

  def discretize(data: Iterable[T]): Seq[Interval] = data match {
    case data: ReversableSortedIterable[T] =>
      val optimalFrequency = countOptimalFrequency(data)
      val intervals = searchIntervals(data, optimalFrequency)
      smoothIntervals(intervals, data)(canItMoveLeft(optimalFrequency))(canItMoveRight(optimalFrequency))
      resolveCutpoints(intervals)
      intervals.iterator.map(_.interval).toList
    case _ => throw new IllegalTypeOfIterable(classOf[ReversableSortedIterable[T]], data.getClass)
  }

}