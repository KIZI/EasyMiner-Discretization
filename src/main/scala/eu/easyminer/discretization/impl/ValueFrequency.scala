package eu.easyminer.discretization.impl

import eu.easyminer.discretization.Consumer
import eu.easyminer.discretization.impl.sorting.SortedProducer

import scala.language.implicitConversions

/**
 * Created by propan on 31. 3. 2017.
 */
case class ValueFrequency[T](value: T, frequency: Int)

object ValueFrequency {

  implicit def sortedProducerToValueFrequencyProducer[T](it: SortedProducer[T])(implicit n: Numeric[T]): Producer[ValueFrequency[T]] = (consumer: Consumer[ValueFrequency[T]]) => {
    var lastValue: Option[ValueFrequency[T]] = None
    for (value <- it) {
      lastValue match {
        case Some(x) if n.equiv(x.value, value) => lastValue = Some(x.copy(frequency = x.frequency + 1))
        case _ =>
          lastValue.foreach(consumer.consume)
          lastValue = Some(ValueFrequency(value, 1))
      }
    }
    lastValue.foreach(consumer.consume)
  }

}