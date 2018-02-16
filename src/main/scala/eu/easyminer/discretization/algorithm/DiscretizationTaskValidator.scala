package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.DiscretizationTask
import eu.easyminer.discretization.algorithm.DiscretizationTaskValidator.Exceptions.InvalidDiscretizationTask
import eu.easyminer.discretization.impl.{AbsoluteSupport, RelativeSupport, Support}
import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask}

/**
  * Created by propan on 2. 4. 2017.
  */
trait DiscretizationTaskValidator[T <: DiscretizationTask] {

  def validate(dt: T): Unit

}

object DiscretizationTaskValidator {

  object Exceptions {

    class InvalidDiscretizationTask(msg: String) extends Exception(msg)

  }

  def throwIfFalse(msg: String)(f: => Boolean): Unit = if (!f) throw new InvalidDiscretizationTask(msg)

  def apply[T <: DiscretizationTask](dt: T)(implicit validator: DiscretizationTaskValidator[T]): Unit = validator.validate(dt)

  implicit val discretizationTaskValidator: DiscretizationTaskValidator[DiscretizationTask] = (dt: DiscretizationTask) => throwIfFalse("Buffer size must be greater than 31 bytes.")(dt.getBufferSize >= 32)

  implicit val equidistanceDiscretizationTaskValidator: DiscretizationTaskValidator[EquidistanceDiscretizationTask] = (dt: EquidistanceDiscretizationTask) => throwIfFalse("Number of bins must be greater than zero.")(dt.getNumberOfBins > 0)

  implicit val equifrequencyDiscretizationTaskValidator: DiscretizationTaskValidator[EquifrequencyDiscretizationTask] = (dt: EquifrequencyDiscretizationTask) => throwIfFalse("Number of bins must be greater than zero.")(dt.getNumberOfBins > 0)

  implicit val equisizeDiscretizationTaskValidator: DiscretizationTaskValidator[EquisizeDiscretizationTask] = (dt: EquisizeDiscretizationTask) => (dt.getMinSupport: Support) match {
    case AbsoluteSupport(s) => throwIfFalse("Absolute support must be greater than 1.")(s > 1)
    case RelativeSupport(s) => throwIfFalse("Relative support must be greater than zero and lower than 1")(s > 0 && s < 1)
  }

}