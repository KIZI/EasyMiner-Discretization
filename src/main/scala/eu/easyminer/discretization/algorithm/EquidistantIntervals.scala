package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.impl.{Interval, IntervalBound}
import eu.easyminer.discretization.impl

/**
 * Created by propan on 17. 3. 2017.
 */
class EquidistantIntervals[T] private[algorithm](bins: Int)(implicit val n: Numeric[T]) extends Discretization[T] {

  def discretize(data: impl.Producer[T]): IndexedSeq[impl.Interval] = data
    .map(x => (x, x))
    .reduceOption((x, y) => n.min(x._1, y._1) -> n.max(x._2, y._2))
    .map(x => n.toDouble(x._1) -> n.toDouble(x._2))
    .iterator
    .flatMap { case (min, max) =>
      val intervalSize = (max - min) / bins
      for (binNumber <- 0 until bins) yield {
        val leftBound = IntervalBound.Inclusive(min + intervalSize * binNumber)
        val rightBound = if (binNumber + 1 == bins) IntervalBound.Inclusive(max) else IntervalBound.Exclusive(leftBound.value + intervalSize)
        Interval(leftBound, rightBound)
      }
    }.toIndexedSeq

}