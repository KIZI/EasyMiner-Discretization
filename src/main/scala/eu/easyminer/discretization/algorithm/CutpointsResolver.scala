package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.impl.{IntervalBound, IntervalFrequency}

/**
  * Created by propan on 31. 3. 2017.
  */
trait CutpointsResolver {

  def resolveCutpoints(intervals: collection.mutable.ArrayBuffer[IntervalFrequency]): Unit = {
    for (i <- 0 until (intervals.length - 1)) {
      val leftInterval = intervals(i)
      val rightInterval = intervals(i + 1)
      val mergedCutPoint = (leftInterval.interval.maxValue.value + rightInterval.interval.minValue.value) / 2.0
      intervals.update(i, leftInterval.copy(interval = leftInterval.interval.copy(maxValue = IntervalBound.Exclusive(mergedCutPoint))))
      intervals.update(i + 1, rightInterval.copy(interval = rightInterval.interval.copy(minValue = IntervalBound.Inclusive(mergedCutPoint))))
    }
  }

}

object CutpointsResolver extends CutpointsResolver