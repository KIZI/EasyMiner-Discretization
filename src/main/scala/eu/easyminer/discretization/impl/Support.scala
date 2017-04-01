package eu.easyminer.discretization.impl

import eu.easyminer.discretization

import scala.language.implicitConversions

/**
  * Created by propan on 31. 3. 2017.
  */
sealed trait Support

object Support {

  implicit def javaSupportToSupport(support: discretization.Support): Support = support match {
    case s: discretization.RelativeSupport => RelativeSupport(s.getSupport)
    case s: discretization.AbsoluteSupport => AbsoluteSupport(s.getSupport)
  }

}

case class RelativeSupport(support: Double) extends Support

case class AbsoluteSupport(support: Int) extends Support