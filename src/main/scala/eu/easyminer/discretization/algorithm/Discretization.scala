package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask}
import eu.easyminer.discretization.{DiscretizationTask, impl}
import DiscretizationTaskValidator.{apply => validate}

/**
  * Created by propan on 16. 3. 2017.
  */
trait Discretization[T] {

  implicit val n: Numeric[T]

  def discretize(data: Traversable[T]): Traversable[impl.Interval]

}

object Discretization {

  object Exceptions {

    class IllegalTypeOfIterable(expected: Class[_], given: Class[_]) extends Exception("Illegal type of input iterable. Expected: " + expected.getSimpleName + ", given: " + given.getSimpleName)

    object UnsupportedDiscretizationTask extends Exception("Unsupported discretization task.")

  }

  def apply[T](dt: DiscretizationTask)(implicit n: Numeric[T]): Discretization[T] = {
    validate(dt)
    dt match {
      case dt: EquidistanceDiscretizationTask =>
        validate(dt)
        new EquidistantIntervals(dt.getNumberOfBins)
      case dt: EquifrequencyDiscretizationTask =>
        validate(dt)
        new EquifrequentIntervals(dt.getNumberOfBins)
      case dt: EquisizeDiscretizationTask =>
        validate(dt)
        new EquisizedIntervals(dt.getMinSupport)
      case _ => throw Exceptions.UnsupportedDiscretizationTask
    }
  }

}