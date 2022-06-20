package eu.easyminer.discretization.impl

import eu.easyminer.discretization.{ReversableSortedProducer, SortedProducer}

import scala.language.implicitConversions

/**
 * Created by propan on 1. 4. 2017.
 */
trait ProducerConversions {

  implicit class PimpedJavaProducer[A <: Number](it: eu.easyminer.discretization.Producer[A]) {
    def asScala[B](implicit n: Numeric[B], numberToScalaNumber: A => B): Producer[B] = {
      it match {
        case it: ReversableSortedProducer[A] => it: sorting.ReversableSortedProducer[B]
        case it: SortedProducer[A] => it: sorting.SortedProducer[B]
        case _ => Producer(it).map(numberToScalaNumber)
      }
    }
  }

}

object ProducerConversions extends ProducerConversions