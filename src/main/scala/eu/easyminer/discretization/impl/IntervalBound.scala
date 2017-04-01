package eu.easyminer.discretization.impl

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait IntervalBound {
  val value: Double
}

case class InclusiveIntervalBound(value: Double) extends IntervalBound

case class ExclusiveIntervalBound(value: Double) extends IntervalBound
