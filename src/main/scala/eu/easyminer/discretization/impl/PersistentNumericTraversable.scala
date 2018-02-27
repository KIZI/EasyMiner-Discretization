package eu.easyminer.discretization.impl

import java.io._

import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentTraversableOps._

/**
  * Created by propan on 17. 3. 2017.
  */
class PersistentNumericTraversable[T] private(col: Traversable[T], file: File)(implicit n: Numeric[T]) extends Traversable[T] {
  implicit private val b2n: Array[Byte] => T = byteArrayToNumber[T]
  def foreach[U](f: T => U): Unit = if (file.exists()) inputStreamTraversable[T](new FileInputStream(file)).foreach(f) else outputStreamTraversable(col, new FileOutputStream(file))
}

object PersistentNumericTraversable {

  def apply[A, B](col: Traversable[A], file: File)(f: Traversable[A] => B)(implicit n: Numeric[A]): B = {
    val pni = new PersistentNumericTraversable(col, file)
    try {
      f(pni)
    } finally {
      file.delete()
    }
  }

}