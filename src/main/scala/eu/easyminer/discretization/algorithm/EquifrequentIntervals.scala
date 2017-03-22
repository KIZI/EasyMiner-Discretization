package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization._
import eu.easyminer.discretization.sorting.SortedIterables

/**
  * Created by propan on 18. 3. 2017.
  */
class EquifrequentIntervals[T](bins: Int)
                              (implicit
                               val n: Numeric[T],
                               iterableToSortedIterables: Iterable[T] => SortedIterables[ValueFrequency[T]])
  extends Discretization[T] {

  def discretize(data: Iterable[T]): Seq[Interval] = ???

  /*{
      val dataCount = data.iterator.map(_.frequency).sum
      val optimalFrequency = math.ceil(dataCount / bins)
      val SortedIterables(asc, desc): SortedIterables[ValueFrequency[T]] = data
      val intervals = new collection.mutable.ArrayBuffer[IntervalFrequency](bins)
      for (value <- asc.iterator) {
        intervals
          .lastOption
          .filter(interval => intervals.length == bins || math.abs(optimalFrequency - (interval.frequency + value.frequency)) < math.abs(optimalFrequency - interval.frequency)) match {
          case Some(interval) => intervals.update(intervals.length - 1, IntervalFrequency(interval.interval.copy(maxValue = InclusiveIntervalBound(n.toDouble(value))), interval.frequency + value.frequency))
          case None =>
            val leftRightBound = InclusiveIntervalBound(n.toDouble(value))
            intervals += IntervalFrequency(Interval(leftRightBound, leftRightBound), value.frequency)
        }
      }
    }*/

}
