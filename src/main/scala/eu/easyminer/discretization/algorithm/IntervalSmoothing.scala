package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.interval.{InclusiveIntervalBound, IntervalFrequency, ValueFrequency}
import eu.easyminer.discretization.sorting.ReversableSortedIterable

/**
  * Created by propan on 31. 3. 2017.
  */
trait IntervalSmoothing {

  def smoothIntervals[T](intervals: collection.mutable.ArrayBuffer[IntervalFrequency], records: ReversableSortedIterable[T])
                        (canItMoveLeft: (ValueFrequency[T], IntervalFrequency, IntervalFrequency) => Boolean)
                        (canItMoveRight: (ValueFrequency[T], IntervalFrequency, IntervalFrequency) => Boolean)
                        (implicit n: Numeric[T]): Unit = {
    val reversedData: Iterable[ValueFrequency[T]] = records.reverse
    val iterates = Iterator.continually {
      reversedData.iterator.foldLeft((Option.empty[ValueFrequency[T]], intervals.length - 1, false)) { case ((lastValue, pointer, isChanged), valueNumeric) =>
        if (pointer > 0) {
          val currentInterval = intervals(pointer)
          val prevInterval = intervals(pointer - 1)
          if (currentInterval.frequency > prevInterval.frequency && n.toDouble(valueNumeric.value) == currentInterval.interval.minValue.value && lastValue.nonEmpty) {
            //right side is greater than left side - move to left!
            //and current record equals right interval "from" border
            //and previous "from" border value of right interval is not empty
            if (canItMoveLeft(valueNumeric, prevInterval, currentInterval)) {
              //we can move values to left interval
              //left(a, b), right(c, d) -> left(a, c), right(e, d) where e is lastValue
              intervals.update(pointer - 1, IntervalFrequency(prevInterval.interval.copy(maxValue = currentInterval.interval.minValue), prevInterval.frequency + valueNumeric.frequency))
              intervals.update(pointer, IntervalFrequency(currentInterval.interval.copy(minValue = InclusiveIntervalBound(n.toDouble(lastValue.get.value))), currentInterval.frequency - valueNumeric.frequency))
              (None, pointer - 1, true)
            } else {
              (None, pointer - 1, isChanged)
            }
          } else if (currentInterval.frequency < prevInterval.frequency && lastValue.exists(x => n.toDouble(x.value) == prevInterval.interval.maxValue.value)) {
            //left side is greater than right side - move to right!
            //and previous record equals left interval "to" border
            //if current record is left interval "from" border then forget all values from left and right interval within next iteration
            //because the left interval will have size 1 - no moves possible
            val nextLastValue = if (prevInterval.interval.minValue.value == n.toDouble(valueNumeric.value)) None else Some(valueNumeric)
            if (canItMoveRight(lastValue.get, prevInterval, currentInterval)) {
              //we can move values to right interval
              //left(a, b), right(c, d) -> left(a, e), right(b, d) where e is current value (record)
              intervals.update(pointer - 1, IntervalFrequency(prevInterval.interval.copy(maxValue = InclusiveIntervalBound(n.toDouble(valueNumeric.value))), prevInterval.frequency - lastValue.get.frequency))
              intervals.update(pointer, IntervalFrequency(currentInterval.interval.copy(minValue = prevInterval.interval.maxValue), currentInterval.frequency + lastValue.get.frequency))
              (nextLastValue, pointer - 1, true)
            } else {
              (nextLastValue, pointer - 1, isChanged)
            }
          } else if (currentInterval.frequency == prevInterval.frequency && n.toDouble(valueNumeric.value) == currentInterval.interval.minValue.value) {
            //intervals are equal -> no moves
            (None, pointer - 1, isChanged)
          } else if (n.toDouble(valueNumeric.value) == prevInterval.interval.minValue.value) {
            //it is the last cutpoint within a current window, we need to change pointer to the next interval
            (None, pointer - 1, isChanged)
          } else {
            //no cutpoint -> go to the next value and safe the current as a last value
            (Some(valueNumeric), pointer, isChanged)
          }
        } else {
          //only one interval remains -> no moves
          (None, pointer, isChanged)
        }
      }._3
    }.takeWhile(_ == true)
    while (iterates.hasNext) iterates.next()
  }

}

object IntervalSmoothing extends IntervalSmoothing