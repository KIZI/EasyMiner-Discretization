package eu.easyminer.discretization.impl

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait Support

object Support {

  case class Relative(support: Double) extends Support

  case class Absolute(support: Int) extends Support

  implicit def javaSupportToSupport(support: discretization.Support): Support = support match {
    case s: discretization.RelativeSupport => Relative(s.getSupport)
    case s: discretization.AbsoluteSupport => Absolute(s.getSupport)
  }

}