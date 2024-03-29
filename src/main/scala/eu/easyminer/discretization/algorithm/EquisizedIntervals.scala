package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.algorithm.CutpointsResolver._
import eu.easyminer.discretization.algorithm.Discretization.Exceptions.IllegalTypeOfTraversable
import eu.easyminer.discretization.algorithm.IntervalSmoothing._
import eu.easyminer.discretization.impl._
import eu.easyminer.discretization.impl.sorting.SortedProducer

/**
  * Created by propan on 31. 3. 2017.
  */
class EquisizedIntervals[T] private[algorithm](minSupport: Support)(implicit val n: Numeric[T]) extends Discretization[T] {

  private def countOptimalFrequency(data: Producer[T]) = minSupport match {
    case Support.Relative(minSupport) => math.ceil(data.size * minSupport).toInt
    case Support.Absolute(minSupport) => minSupport
  }

  private def searchIntervals(data: Producer[ValueFrequency[T]], optimalFrequency: Int) = {
    val intervals = new collection.mutable.ArrayBuffer[Interval.WithFrequency]()
    for (value <- data) {
      intervals
        .lastOption
        .filter(interval => interval.frequency < optimalFrequency) match {
        case Some(interval) => intervals.update(intervals.length - 1,  interval.copy(maxValue = IntervalBound.Inclusive(n.toDouble(value.value)), frequency = interval.frequency + value.frequency))
        case None =>
          val leftRightBound = IntervalBound.Inclusive(n.toDouble(value.value))
          intervals += Interval(leftRightBound, leftRightBound, value.frequency)
      }
    }
    if (intervals.length > 1) {
      intervals.lastOption.filter(_.frequency < optimalFrequency) foreach { lastInterval =>
        val prevLastInterval = intervals(intervals.length - 2)
        intervals.update(intervals.length - 2, Interval(prevLastInterval.minValue, lastInterval.maxValue, lastInterval.frequency + prevLastInterval.frequency))
        intervals.trimToSize()
      }
    }
    intervals
  }

  private def canItMoveLeft(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: Interval.WithFrequency, rightInterval: Interval.WithFrequency) = {
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val decreasedIntervalFreqency = rightInterval.frequency - movedValue.frequency
    val nextDifference = math.abs(decreasedIntervalFreqency - (leftInterval.frequency + movedValue.frequency))
    decreasedIntervalFreqency >= optimalFrequency && nextDifference < currentDifference
  }

  private def canItMoveRight(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: Interval.WithFrequency, rightInterval: Interval.WithFrequency) = {
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val decreasedIntervalFreqency = leftInterval.frequency - movedValue.frequency
    val nextDifference = math.abs((rightInterval.frequency + movedValue.frequency) - decreasedIntervalFreqency)
    decreasedIntervalFreqency >= optimalFrequency && nextDifference < currentDifference
  }

  def discretize(data: Producer[T]): IndexedSeq[Interval.WithFrequency] = data match {
    case data: SortedProducer[T] =>
      val optimalFrequency = countOptimalFrequency(data)
      val intervals = searchIntervals(data, optimalFrequency)
      smoothIntervals(intervals, data, 1000000)(canItMoveLeft(optimalFrequency))(canItMoveRight(optimalFrequency))
      resolveCutpoints(intervals.asInstanceOf[collection.mutable.ArrayBuffer[Interval]])
      intervals.toIndexedSeq
    case _ => throw new IllegalTypeOfTraversable(classOf[SortedProducer[T]], data.getClass)
  }

}