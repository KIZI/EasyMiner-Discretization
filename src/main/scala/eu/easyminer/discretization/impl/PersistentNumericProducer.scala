package eu.easyminer.discretization.impl

import eu.easyminer.discretization.Consumer

import java.io._
import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentProducerOps._

/**
 * Created by propan on 17. 3. 2017.
 */
class PersistentNumericProducer[T] private(col: Producer[T], file: File)(implicit n: Numeric[T]) extends Producer[T] {
  implicit private val b2n: Array[Byte] => T = byteArrayToNumber[T]

  override def knownSize: Int = if (file.exists()) -1 else col.knownSize

  def produce(consumer: Consumer[T]): Unit = {
    if (file.exists()) inputStreamProducer[T](new FileInputStream(file)).foreach(consumer.consume) else outputStreamProducer(col, new FileOutputStream(file)).foreach(consumer.consume)
  }
}

object PersistentNumericProducer {

  def apply[A, B](col: Producer[A], file: File)(f: Producer[A] => B)(implicit n: Numeric[A]): B = {
    val pni = new PersistentNumericProducer(col, file)
    try {
      f(pni)
    } finally {
      file.delete()
    }
  }

}