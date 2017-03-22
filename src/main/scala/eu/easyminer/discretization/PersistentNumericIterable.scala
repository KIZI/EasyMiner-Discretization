package eu.easyminer.discretization

import java.io._

import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentIteratorOps._

/**
  * Created by propan on 17. 3. 2017.
  */
class PersistentNumericIterable[T] private(it: Iterator[T], file: File)(implicit n: Numeric[T]) extends Iterable[T] {

  implicit private val b2n = byteArrayToNumber[T] _

  def iterator: Iterator[T] = if (it.hasNext) outputStreamIterator(it, new FileOutputStream(file)) else inputStreamIterator[T](new FileInputStream(file))

}

object PersistentNumericIterable {

  def apply[T](it: Iterator[T], file: File)(implicit n: Numeric[T]): Iterable[T] = new PersistentNumericIterable(it, file)

}