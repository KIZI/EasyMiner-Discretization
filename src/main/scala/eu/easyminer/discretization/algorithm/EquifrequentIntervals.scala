package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.algorithm.CutpointsResolver._
import eu.easyminer.discretization.algorithm.Discretization.Exceptions.IllegalTypeOfIterable
import eu.easyminer.discretization.algorithm.IntervalSmoothing._
import eu.easyminer.discretization.impl.sorting.ReversableSortedIterable
import eu.easyminer.discretization.impl.{InclusiveIntervalBound, Interval, IntervalFrequency, ValueFrequency}

/**
  * Created by propan on 18. 3. 2017.
  */
class EquifrequentIntervals[T] private[algorithm](bins: Int)(implicit val n: Numeric[T]) extends Discretization[T] {

  private def countOptimalFrequency(data: Iterable[T]) = {
    val dataCount = data.iterator.size
    math.ceil(dataCount / bins).toInt
  }

  private def searchIntervals(data: Iterable[ValueFrequency[T]], optimalFrequency: Int) = {
    val intervals = new collection.mutable.ArrayBuffer[IntervalFrequency](bins)
    for (value <- data.iterator) {
      intervals
        .lastOption
        .filter(interval => intervals.length == bins || math.abs(optimalFrequency - (interval.frequency + value.frequency)) < math.abs(optimalFrequency - interval.frequency)) match {
        case Some(interval) => intervals.update(intervals.length - 1, IntervalFrequency(interval.interval.copy(maxValue = InclusiveIntervalBound(n.toDouble(value.value))), interval.frequency + value.frequency))
        case None =>
          val leftRightBound = InclusiveIntervalBound(n.toDouble(value.value))
          intervals += IntervalFrequency(Interval(leftRightBound, leftRightBound), value.frequency)
      }
    }
    intervals
  }

  //                                     * - optimal                          * - optimal
  //before: prevInterval[f1, t1;----______] < currentInterval(f2, t2;--------------)
  //                                ^-------------<-move-<-----------^^^^ <- moved part is valueNumeric == f2
  //after:  prevInterval[f1, t1;--------__] < currentInterval(f2, t2;----------)
  //currentScore = ----_____* + ---------*----
  //                   ^^^^^^ +          ^^^^^ = distance current frequency from optimal = 6 + 5 = 11
  //nextScore    = --------_* + ---------*
  //                       ^^ +          ^ = 2 + 1 = 3
  //if nextScore is lower than currentScore, make shift!
  //go to the next interval
  private def canItMoveLeft(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: IntervalFrequency, rightInterval: IntervalFrequency) = {
    val currentScore = math.abs(optimalFrequency - rightInterval.frequency) + math.abs(optimalFrequency - leftInterval.frequency)
    val nextScore = math.abs(optimalFrequency - (rightInterval.frequency - movedValue.frequency)) + math.abs(optimalFrequency - (leftInterval.frequency + movedValue.frequency))
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val nextDifference = math.abs((rightInterval.frequency - movedValue.frequency) - (leftInterval.frequency + movedValue.frequency))
    nextScore <= currentScore && nextDifference < currentDifference
  }

  //before: prevInterval[f1, t1;--------------] > currentInterval(f2, t2;----______)
  //                                      ^^^^--------->-move->--------------^ <- moved part is lastValue == t1
  private def canItMoveRight(optimalFrequency: Int)(movedValue: ValueFrequency[T], leftInterval: IntervalFrequency, rightInterval: IntervalFrequency) = {
    val currentScore = math.abs(optimalFrequency - rightInterval.frequency) + math.abs(optimalFrequency - leftInterval.frequency)
    val nextScore = math.abs(optimalFrequency - (rightInterval.frequency + movedValue.frequency)) + math.abs(optimalFrequency - (leftInterval.frequency - movedValue.frequency))
    val currentDifference = math.abs(rightInterval.frequency - leftInterval.frequency)
    val nextDifference = math.abs((rightInterval.frequency + movedValue.frequency) - (leftInterval.frequency - movedValue.frequency))
    nextScore <= currentScore && nextDifference < currentDifference
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