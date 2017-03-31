package eu.easyminer.discretization

/**
  * Created by propan on 16. 3. 2017.
  */
trait Discretization[T] {

  implicit val n: Numeric[T]

  def discretize(data: Iterable[T]): Seq[interval.Interval]

}

object Discretization {

  object Exceptions {

    class IllegalTypeOfIterable(expected: Class[_], given: Class[_]) extends Exception("Illegal type of input iterable. Expected: " + expected.getSimpleName + ", given: " + given.getSimpleName)

  }

}