package eu.easyminer.discretization.impl

import java.io.File

import eu.easyminer.discretization
import eu.easyminer.discretization.algorithm.{Discretization, EquidistantIntervals, EquifrequentIntervals, EquisizedIntervals}
import eu.easyminer.discretization.impl.IterableConversions._
import eu.easyminer.discretization.impl.sorting.{ReversableSortedTraversable, SortedInMemoryNumericTraversable, SortedPersistentNumericTraversable}
import eu.easyminer.discretization.{Discretizable, DiscretizationTask}

import scala.language.implicitConversions
import scala.util.Random


/**
  * Created by propan on 31. 3. 2017.
  */
object DefaultDiscretization extends Discretizable {

  private def javaNumberToScalaNumber[A <: Number, B](number: A)(implicit n: Numeric[B]): B = number.asInstanceOf[B]

  private def doWithNumeric[A <: Number, B](discretizationTask: DiscretizationTask, data: java.lang.Iterable[A])(implicit n: Numeric[B]): Array[discretization.Interval] = {
    lazy val file = Stream.continually(new File(Random.alphanumeric.take(8).mkString)).find(!_.exists()).get
    lazy val directory = new File("./")
    implicit val sn: A => B = javaNumberToScalaNumber[A, B]
    implicit val c: java.util.Iterator[A] => Iterator[B] = javaIteratorToIterator[A, B]
    val dt = Discretization(discretizationTask)
    dt match {
      case dt: EquidistantIntervals[B] => dt.discretize(data.asScala)
      case _: EquifrequentIntervals[B] | _: EquisizedIntervals[B] => data match {
        case data: discretization.SortedIterable[A] with discretization.PersistentIterable[A] =>
          SortedPersistentNumericTraversable[B, Seq[Interval]](data, file)(dt.discretize)
        case data: discretization.InMemoryIterable[A] =>
          dt.discretize(SortedInMemoryNumericTraversable(data.iterator(), discretizationTask.getBufferSize))
        case data: discretization.ReversableSortedIterable[A] =>
          dt.discretize(data: ReversableSortedTraversable[B])
        case data: discretization.SortedIterable[A] =>
          SortedPersistentNumericTraversable[B, Seq[Interval]](data, file)(dt.discretize)
        case _ =>
          SortedPersistentNumericTraversable[B, Seq[Interval]](data.iterator(), directory, discretizationTask.getBufferSize)(dt.discretize)
      }
      case _ => Array()
    }
  }

  def discretize[T <: Number](discretizationTask: DiscretizationTask, data: java.lang.Iterable[T], clazz: Class[T]): Array[discretization.Interval] = {
    val JavaInt = classOf[java.lang.Integer]
    val JavaShort = classOf[java.lang.Short]
    val JavaByte = classOf[java.lang.Byte]
    val JavaFloat = classOf[java.lang.Float]
    val JavaDouble = classOf[java.lang.Double]
    val JavaLong = classOf[java.lang.Long]
    clazz match {
      case `JavaInt` => doWithNumeric[T, Int](discretizationTask, data)
      case `JavaShort` => doWithNumeric[T, Short](discretizationTask, data)
      case `JavaByte` => doWithNumeric[T, Byte](discretizationTask, data)
      case `JavaFloat` => doWithNumeric[T, Float](discretizationTask, data)
      case `JavaDouble` => doWithNumeric[T, Double](discretizationTask, data)
      case `JavaLong` => doWithNumeric[T, Long](discretizationTask, data)
      case _ => throw new IllegalArgumentException
    }
  }

}
