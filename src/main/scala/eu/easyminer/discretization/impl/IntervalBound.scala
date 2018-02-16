package eu.easyminer.discretization.impl

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait IntervalBound {
  val value: Double
}

object IntervalBound {

  case class Inclusive(value: Double) extends IntervalBound

  case class Exclusive(value: Double) extends IntervalBound

}