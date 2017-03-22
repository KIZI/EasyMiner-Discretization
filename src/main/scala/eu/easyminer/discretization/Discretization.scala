package eu.easyminer.discretization

/**
  * Created by propan on 16. 3. 2017.
  */
trait Discretization[T] {

  implicit val n: Numeric[T]

  def discretize(data: Iterable[T]): Seq[Interval]

}
