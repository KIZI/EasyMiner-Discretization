package eu.easyminer.discretization

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait Support

case class RelativeSupport(support: Double) extends Support

case class AbsoluteSupport(support: Int) extends Support