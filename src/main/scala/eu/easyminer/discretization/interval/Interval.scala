package eu.easyminer.discretization.interval

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 16. 3. 2017.
  */
case class Interval(minValue: IntervalBound, maxValue: IntervalBound)

object Interval {

  implicit def intervalToJavaInterval(interval: Interval): discretization.Interval = new discretization.Interval {
    def getLeftBoundValue: java.lang.Double = interval.minValue.value

    def getRightBoundValue: java.lang.Double = interval.maxValue.value

    def isLeftBoundClosed: java.lang.Boolean = interval.minValue.isInstanceOf[InclusiveIntervalBound]

    def isRightBoundClosed: java.lang.Boolean = interval.maxValue.isInstanceOf[InclusiveIntervalBound]

    def isLeftBoundOpened: java.lang.Boolean = !isLeftBoundClosed

    def isRightBoundOpened: java.lang.Boolean = !isRightBoundClosed
  }

}

case class IntervalFrequency(interval: Interval, frequency: Int)