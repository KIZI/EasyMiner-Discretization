package eu.easyminer.discretization.impl

import java.lang

import eu.easyminer.discretization

/**
  * Created by propan on 16. 3. 2017.
  */
sealed trait Interval extends discretization.Interval {
  val minValue: IntervalBound
  val maxValue: IntervalBound

  def getLeftBoundValue: lang.Double = minValue.value

  def getRightBoundValue: lang.Double = maxValue.value

  def isLeftBoundOpened: lang.Boolean = minValue.isInstanceOf[IntervalBound.Exclusive]

  def isRightBoundOpened: lang.Boolean = maxValue.isInstanceOf[IntervalBound.Exclusive]

  def isLeftBoundClosed: lang.Boolean = !isLeftBoundOpened

  def isRightBoundClosed: lang.Boolean = !isRightBoundOpened

  def withMinValue(minValue: IntervalBound): Interval

  def withMaxValue(maxValue: IntervalBound): Interval

  def isInInterval(value: Double): lang.Boolean = {
    val isGtMinValue = minValue match {
      case IntervalBound.Inclusive(x) => value >= x
      case IntervalBound.Exclusive(x) => value > x
    }
    val isLtMaxValue = maxValue match {
      case IntervalBound.Inclusive(x) => value <= x
      case IntervalBound.Exclusive(x) => value < x
    }
    isGtMinValue && isLtMaxValue
  }
}

object Interval {

  def apply(minValue: IntervalBound, maxValue: IntervalBound): Interval.Simple = Simple(minValue, maxValue)

  def apply(minValue: IntervalBound, maxValue: IntervalBound, frequency: Int): Interval.WithFrequency = WithFrequency(minValue, maxValue, frequency)

  case class Simple(minValue: IntervalBound, maxValue: IntervalBound) extends Interval {
    def withMinValue(minValue: IntervalBound): Interval = copy(minValue = minValue)

    def withMaxValue(maxValue: IntervalBound): Interval = copy(maxValue = maxValue)
  }

  case class WithFrequency(minValue: IntervalBound, maxValue: IntervalBound, frequency: Int) extends Interval {
    def withMinValue(minValue: IntervalBound): Interval = copy(minValue = minValue)

    def withMaxValue(maxValue: IntervalBound): Interval = copy(maxValue = maxValue)
  }

}