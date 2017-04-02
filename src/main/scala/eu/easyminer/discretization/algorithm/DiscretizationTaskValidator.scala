package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.DiscretizationTask
import eu.easyminer.discretization.algorithm.DiscretizationTaskValidator.Exceptions.InvalidDiscretizationTask
import eu.easyminer.discretization.impl.{AbsoluteSupport, RelativeSupport, Support}
import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask}


/**
  * Created by propan on 2. 4. 2017.
  */
trait DiscretizationTaskValidator[T <: DiscretizationTask] {

  protected def throwIfFalse(msg: String)(f: => Boolean) = if (!f) throw new InvalidDiscretizationTask(msg)

  def validate(dt: T): Unit

}

object DiscretizationTaskValidator {

  object Exceptions {

    class InvalidDiscretizationTask(msg: String) extends Exception(msg)

  }

  def apply[T <: DiscretizationTask](dt: T)(implicit validator: DiscretizationTaskValidator[T]) = validator.validate(dt)

  implicit val discretizationTaskValidator = new DiscretizationTaskValidator[DiscretizationTask] {
    def validate(dt: DiscretizationTask): Unit = throwIfFalse("Buffer size must be greater than 31 bytes.")(dt.getBufferSize >= 32)
  }

  implicit val equidistanceDiscretizationTaskValidator = new DiscretizationTaskValidator[EquidistanceDiscretizationTask] {
    def validate(dt: EquidistanceDiscretizationTask): Unit = throwIfFalse("Number of bins must be greater than zero.")(dt.getNumberOfBins > 0)
  }

  implicit val equifrequencyDiscretizationTaskValidator = new DiscretizationTaskValidator[EquifrequencyDiscretizationTask] {
    def validate(dt: EquifrequencyDiscretizationTask): Unit = throwIfFalse("Number of bins must be greater than zero.")(dt.getNumberOfBins > 0)
  }

  implicit val equisizeDiscretizationTaskValidator = new DiscretizationTaskValidator[EquisizeDiscretizationTask] {
    def validate(dt: EquisizeDiscretizationTask): Unit = (dt.getMinSupport: Support) match {
      case AbsoluteSupport(s) => throwIfFalse("Absolute support must be greater than 1.")(s > 1)
      case RelativeSupport(s) => throwIfFalse("Relative support must be greater than zero and lower than 1")(s > 0 && s < 1)
    }
  }

}