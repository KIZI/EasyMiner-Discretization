package eu.easyminer.discretization.impl

import java.lang

import eu.easyminer.discretization

/**
  * Created by propan on 16. 3. 2017.
  */
case class Interval(minValue: IntervalBound, maxValue: IntervalBound) extends discretization.Interval {
  def getLeftBoundValue: lang.Double = minValue.value

  def getRightBoundValue: lang.Double = maxValue.value

  def isLeftBoundOpened: lang.Boolean = minValue.isInstanceOf[IntervalBound.Exclusive]

  def isRightBoundOpened: lang.Boolean = maxValue.isInstanceOf[IntervalBound.Exclusive]

  def isLeftBoundClosed: lang.Boolean = !isLeftBoundOpened

  def isRightBoundClosed: lang.Boolean = !isRightBoundOpened

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