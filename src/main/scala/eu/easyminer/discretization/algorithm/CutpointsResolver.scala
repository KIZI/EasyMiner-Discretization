package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.impl.{Interval, IntervalBound}

/**
  * Created by propan on 31. 3. 2017.
  */
trait CutpointsResolver {

  def resolveCutpoints(intervals: collection.mutable.ArrayBuffer[Interval]): Unit = {
    for (i <- 0 until (intervals.length - 1)) {
      val leftInterval = intervals(i)
      val rightInterval = intervals(i + 1)
      val mergedCutPoint = (leftInterval.maxValue.value + rightInterval.minValue.value) / 2.0
      intervals.update(i, leftInterval.withMaxValue(IntervalBound.Exclusive(mergedCutPoint)))
      intervals.update(i + 1, rightInterval.withMinValue(IntervalBound.Inclusive(mergedCutPoint)))
    }
  }

}

object CutpointsResolver extends CutpointsResolver