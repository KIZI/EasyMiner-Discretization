package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.impl
import eu.easyminer.discretization.impl.{Interval, IntervalBound}

/**
  * Created by propan on 17. 3. 2017.
  */
class EquidistantIntervals[T] private[algorithm](bins: Int)(implicit val n: Numeric[T]) extends Discretization[T] {

  def discretize(data: Traversable[T]): IndexedSeq[impl.Interval] = data.view
    .map(x => (x, x))
    .reduceOption((x, y) => n.min(x._1, y._1) -> n.max(x._2, y._2))
    .map(x => n.toDouble(x._1) -> n.toDouble(x._2))
    .toIterator
    .flatMap { case (min, max) =>
      val intervalSize = (max - min) / bins
      for (binNumber <- 0 until bins) yield {
        val leftBound = IntervalBound.Inclusive(min + intervalSize * binNumber)
        val rightBound = if (binNumber + 1 == bins) IntervalBound.Inclusive(max) else IntervalBound.Exclusive(leftBound.value + intervalSize)
        Interval(leftBound, rightBound)
      }
    }.toIndexedSeq

}