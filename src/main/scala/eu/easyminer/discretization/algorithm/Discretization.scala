package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask, EquisizeTreeDiscretizationTask}
import eu.easyminer.discretization.{DiscretizationTask, impl}
import DiscretizationTaskValidator.{apply => validate}
import eu.easyminer.discretization.impl.Producer

/**
  * Created by propan on 16. 3. 2017.
  */
trait Discretization[T] {

  implicit val n: Numeric[T]

  def discretize(data: Producer[T]): IndexedSeq[impl.Interval]

}

object Discretization {

  object Exceptions {

    class IllegalTypeOfTraversable(expected: Class[_], given: Class[_]) extends Exception("Illegal type of input traversable. Expected: " + expected.getSimpleName + ", given: " + given.getSimpleName)

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
      case dt: EquisizeTreeDiscretizationTask =>
        validate(dt)
        new EquisizedIntervalsTree[T](dt.getMinSupport, dt.getArity, dt.inParallel())
      case _ => throw Exceptions.UnsupportedDiscretizationTask
    }
  }

}