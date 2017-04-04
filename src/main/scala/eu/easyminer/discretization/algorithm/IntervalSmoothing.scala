package eu.easyminer.discretization.algorithm

import java.util

import eu.easyminer.discretization.impl.sorting.SortedIterable
import eu.easyminer.discretization.impl.{InclusiveIntervalBound, IntervalFrequency, ValueFrequency}
import eu.easyminer.discretization.util.NumericByteArray._

/**
  * Created by propan on 31. 3. 2017.
  */
trait IntervalSmoothing {

  def smoothIntervals[T](intervals: collection.mutable.ArrayBuffer[IntervalFrequency], records: SortedIterable[T], bufferSize: Int)
                        (canItMoveLeft: (ValueFrequency[T], IntervalFrequency, IntervalFrequency) => Boolean)
                        (canItMoveRight: (ValueFrequency[T], IntervalFrequency, IntervalFrequency) => Boolean)
                        (implicit n: Numeric[T]): Unit = {
    if (bufferSize < 32) throw new IllegalArgumentException("Buffer size for smoothing must be greater than 31 bytes.")
    //input data are converted into ValueFrequency - it is aggregated distinct values with their count
    val groupedData: Iterable[ValueFrequency[T]] = records
    //values buffer for faster smoothing iteration
    val buffer = new util.LinkedList[ValueFrequency[T]]()
    //miximal number of values in the buffer
    val maxBufferSize = bufferSize / n.zero.length
    //smooth until there are no interval changes
    val iterates = Iterator.continually {
      //within each smoothing iteration all sorted data are iterated
      groupedData.iterator.foldLeft(0, false) { case ((pointer, isChanged), currentValue) =>
        if (pointer < intervals.length - 1) {
          //we have two intervals to compare
          val leftInterval = intervals(pointer)
          val rightInterval = intervals(pointer + 1)
          //load previous value - it is peek of the buffer
          val prevValue = if (buffer.size() > 0) Some(buffer.getFirst) else None
          //add current value into the buffer
          buffer.addFirst(currentValue)
          //if the buffer size is full + 1, then remove last value
          if (buffer.size > maxBufferSize) buffer.removeLast()
          //this method returns next pointer and clear the buffer
          def nextPointerAndClearBuffer() = {
            buffer.clear()
            pointer + 1
          }
          //this method moves right interval border into the left interval
          def moveToLeft() = {
            //new left interval has right border as prevValue = add prev value into the left interval
            intervals.update(pointer, IntervalFrequency(leftInterval.interval.copy(maxValue = rightInterval.interval.minValue), leftInterval.frequency + prevValue.get.frequency))
            //new right interval has left border as currentValue = remove prev value from the right interval
            intervals.update(pointer + 1, IntervalFrequency(rightInterval.interval.copy(minValue = InclusiveIntervalBound(n.toDouble(currentValue.value))), rightInterval.frequency - prevValue.get.frequency))
          }
          //this method moves left interval borders into the right interval
          //it moves border from all items in the buffer until condition
          @scala.annotation.tailrec
          def moveToRight(leftInterval: IntervalFrequency, rightInterval: IntervalFrequency): Unit = if (leftInterval.frequency > rightInterval.frequency && buffer.size() > 1 && leftInterval.interval.maxValue.value > leftInterval.interval.minValue.value) {
            //COND1: left interval is greater than right
            //COND2: buffer has minimal two values
            //COND3: left interval max values is greater then left interval min values - this prevents to move values which are not contain in the left interval
            //load two values from the buffer and remove peek from the buffer
            val currentValue = buffer.pollFirst()
            val prevValue = buffer.getFirst
            //new left interval has right border as prevValue = delete current from the left interval
            val newLeftInterval = IntervalFrequency(leftInterval.interval.copy(maxValue = InclusiveIntervalBound(n.toDouble(prevValue.value))), leftInterval.frequency - currentValue.frequency)
            //new right interval has left border as currentValue = add current into the right interval
            val newRightInterval = IntervalFrequency(rightInterval.interval.copy(minValue = leftInterval.interval.maxValue), rightInterval.frequency + currentValue.frequency)
            //do it again
            moveToRight(newLeftInterval, newRightInterval)
          } else {
            //conditions are not satisfied = no moves are possible
            //update intervals in the result list
            intervals.update(pointer, leftInterval)
            intervals.update(pointer + 1, rightInterval)
          }
          if (rightInterval.frequency > leftInterval.frequency && prevValue.exists(x => n.toDouble(x.value) == rightInterval.interval.minValue.value)) {
            //right side is greater than left side - move to left!
            //and prev value equals right interval "from" border
            if (canItMoveLeft(prevValue.get, leftInterval, rightInterval)) {
              //we can move values to left interval
              //left(a, b), right(c, d) -> left(a, c), right(e, d) where e is current value
              moveToLeft()
              //if current value is right interval max value then clear the buffer and move the window pointer
              //next value afrer current value will not be contained in the right interval therefore we need to move intervals window
              //else keep the current window and in the next iteration we will try to move other values to the left interval
              val nextPointer = if (n.toDouble(currentValue.value) == rightInterval.interval.maxValue.value) nextPointerAndClearBuffer() else pointer
              (nextPointer, true)
            } else {
              //no moves possible, clear buffer and move window pointer
              (nextPointerAndClearBuffer(), isChanged)
            }
          } else if (rightInterval.frequency < leftInterval.frequency && n.toDouble(currentValue.value) == leftInterval.interval.maxValue.value && prevValue.nonEmpty) {
            //left side is greater than right side - move to right!
            //and previous record is not empty
            //and current record is left interval max value
            if (canItMoveRight(currentValue, leftInterval, rightInterval)) {
              //we can move values to right interval
              //left(a, b), right(c, d) -> left(a, e), right(b, d) where e is prev value (record)
              moveToRight(leftInterval, rightInterval)
              //after several moves to to right, clear the buffer and move window pointer
              (nextPointerAndClearBuffer(), true)
            } else {
              //no moves possible, clear buffer and move window pointer
              (nextPointerAndClearBuffer(), isChanged)
            }
          } else if (leftInterval.frequency == rightInterval.frequency && n.toDouble(currentValue.value) == leftInterval.interval.maxValue.value) {
            //intervals are equal -> no moves
            (nextPointerAndClearBuffer(), isChanged)
          } else if (n.toDouble(currentValue.value) == rightInterval.interval.maxValue.value) {
            //it is the last cutpoint within a current window, we need to change pointer to the next interval
            (pointer + 1, isChanged)
          } else {
            //no cutpoint -> go to the next value and safe the current as a last value
            (pointer, isChanged)
          }
        } else {
          //only one interval remains -> no moves
          (pointer, isChanged)
        }
      }._2
    }.takeWhile(_ == true)
    while (iterates.hasNext) iterates.next()
  }

}

object IntervalSmoothing extends IntervalSmoothing