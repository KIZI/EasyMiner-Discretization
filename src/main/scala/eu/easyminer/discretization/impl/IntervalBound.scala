package eu.easyminer.discretization.impl

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait IntervalBound {
  val value: Double

  def >|:(that: Double): Boolean

  def <|:(that: Double): Boolean
}

object IntervalBound {

  case class Inclusive(value: Double) extends IntervalBound {
    def >|:(that: Double): Boolean = that > value

    def <|:(that: Double): Boolean = that < value
  }

  case class Exclusive(value: Double) extends IntervalBound {
    def >|:(that: Double): Boolean = that >= value

    def <|:(that: Double): Boolean = that <= value
  }

}