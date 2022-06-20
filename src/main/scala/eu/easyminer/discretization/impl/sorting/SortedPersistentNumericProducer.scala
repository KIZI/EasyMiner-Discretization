package eu.easyminer.discretization.impl.sorting

import eu.easyminer.discretization.Consumer
import eu.easyminer.discretization.impl.{PersistentNumericProducer, Producer}
import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentProducerOps._
import eu.easyminer.discretization.util.ReversedFileInputStream

import java.io.{File, FileInputStream}

/**
 * Created by propan on 19. 3. 2017.
 */
object SortedPersistentNumericProducer {

  def apply[A, B](col: Producer[A], directory: File, bufferSize: Int)(f: ReversableSortedProducer[A] => B)(implicit n: Numeric[A]): B = {
    implicit val b2n: Array[Byte] => A = byteArrayToNumber[A]
    val ems = new ExternalMergeSort(bufferSize)
    lazy val sortedFile = ems.sort(col, directory)
    try {
      val rst = new ReversableSortedProducer[A](
        inputStreamProducer[A](new FileInputStream(sortedFile)),
        inputStreamProducer[A](new ReversedFileInputStream(sortedFile, n.zero.length))
      )
      f(rst)
    } finally {
      sortedFile.delete()
    }
  }

  def apply[A, B](col: SortedProducer[A], file: File)(f: ReversableSortedProducer[A] => B)(implicit n: Numeric[A]): B = PersistentNumericProducer(col, file) { col =>
    implicit val b2n: Array[Byte] => A = byteArrayToNumber[A]
    val rst = new ReversableSortedProducer[A](
      col,
      (consumer: Consumer[A]) => {
        if (!file.exists()) col.foreach(_ => ())
        inputStreamProducer(new ReversedFileInputStream(file, n.zero.length)).foreach(consumer.consume)
      }
    )
    f(rst)
  }

}
