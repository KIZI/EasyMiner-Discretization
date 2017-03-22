package eu.easyminer.discretization.sorting

import java.io.{File, FileInputStream}

import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentIteratorOps._
import eu.easyminer.discretization.util.ReversedFileInputStream

/**
  * Created by propan on 19. 3. 2017.
  */
class SortedPersistentNumericIterable[T] private(it: Iterator[T], directory: File, val bufferSize: Int)(implicit n: Numeric[T]) extends Iterable[T] with ExternalMergeSort {

  lazy val sortedFile = sort(it, directory)

  implicit private val b2n = byteArrayToNumber[T] _

  def iterator: Iterator[T] = inputStreamIterator[T](new FileInputStream(sortedFile))

  def reverse: Iterable[T] = new Iterable[T] {
    def iterator: Iterator[T] = inputStreamIterator[T](new ReversedFileInputStream(sortedFile, n.zero.length))
  }

}

object SortedPersistentNumericIterable {

  def apply[T](it: Iterator[T], directory: File, bufferSize: Int)(implicit n: Numeric[T]): (Iterable[T], Iterable[T]) = {
    val spni = new SortedPersistentNumericIterable(it, directory, bufferSize)
    (spni, spni.reverse)
  }

}
