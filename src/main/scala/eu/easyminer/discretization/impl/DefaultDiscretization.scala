package eu.easyminer.discretization.impl

import java.io.File
import eu.easyminer.discretization
import eu.easyminer.discretization.algorithm.{Discretization, EquidistantIntervals, EquifrequentIntervals, EquisizedIntervals, EquisizedIntervalsTree}
import eu.easyminer.discretization.impl.ProducerConversions._
import eu.easyminer.discretization.impl.sorting.{SortedInMemoryNumericProducer, SortedPersistentNumericProducer}
import eu.easyminer.discretization.{Discretizable, DiscretizationTask, InMemoryProducer, PersistentProducer, ReversableSortedProducer, SortedProducer}

import scala.language.implicitConversions
import scala.util.Random


/**
 * Created by propan on 31. 3. 2017.
 */
object DefaultDiscretization extends Discretizable {

  private def javaNumberToScalaNumber[A <: Number, B](number: A)(implicit n: Numeric[B]): B = number.asInstanceOf[B]

  private def doWithNumeric[A <: Number, B](discretizationTask: DiscretizationTask, data: discretization.Producer[A])(implicit n: Numeric[B]): Array[discretization.Interval] = {
    lazy val file = Iterator.continually(new File(Random.alphanumeric.take(8).mkString)).find(!_.exists()).get
    lazy val directory = new File("./")
    implicit val sn: A => B = javaNumberToScalaNumber[A, B]
    //implicit val c: java.util.Iterator[A] => Iterator[B] = javaIteratorToIterator[A, B]
    val dt = Discretization(discretizationTask)
    dt match {
      case dt: EquidistantIntervals[B] => dt.discretize(data.asScala).toArray
      case _: EquifrequentIntervals[B] | _: EquisizedIntervals[B] | _: EquisizedIntervalsTree[B] => data match {
        case data: SortedProducer[A] with PersistentProducer[A] =>
          SortedPersistentNumericProducer[B, IndexedSeq[Interval]](data, file)(dt.discretize).toArray
        case data: InMemoryProducer[A] =>
          dt.discretize(SortedInMemoryNumericProducer(data.asScala, discretizationTask.getBufferSize)).toArray
        case data: ReversableSortedProducer[A] =>
          dt.discretize(data.asScala).toArray
        case data: SortedProducer[A] =>
          dt.discretize(data.asScala).toArray
        case _ =>
          SortedPersistentNumericProducer[B, IndexedSeq[Interval]](data.asScala, directory, discretizationTask.getBufferSize)(dt.discretize).toArray
      }
      case _ => Array()
    }
  }

  def discretize[T <: Number](discretizationTask: DiscretizationTask, data: discretization.Producer[T], clazz: Class[T]): Array[discretization.Interval] = {
    if (clazz == classOf[java.lang.Integer]) {
      doWithNumeric[T, Int](discretizationTask, data)
    } else if (clazz == classOf[java.lang.Short]) {
      doWithNumeric[T, Short](discretizationTask, data)
    } else if (clazz == classOf[java.lang.Byte]) {
      doWithNumeric[T, Byte](discretizationTask, data)
    } else if (clazz == classOf[java.lang.Float]) {
      doWithNumeric[T, Float](discretizationTask, data)
    } else if (clazz == classOf[java.lang.Double]) {
      doWithNumeric[T, Double](discretizationTask, data)
    } else if (clazz == classOf[java.lang.Long]) {
      doWithNumeric[T, Long](discretizationTask, data)
    } else {
      throw new IllegalArgumentException
    }
  }

}