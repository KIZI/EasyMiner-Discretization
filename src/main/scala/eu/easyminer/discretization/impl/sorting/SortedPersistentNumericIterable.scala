package eu.easyminer.discretization.impl.sorting

import java.io.{File, FileInputStream}

import eu.easyminer.discretization.impl.PersistentNumericIterable
import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentIteratorOps._
import eu.easyminer.discretization.util.ReversedFileInputStream

/**
  * Created by propan on 19. 3. 2017.
  */
class SortedPersistentNumericIterable[T] private(it: Iterator[T], directory: File, val bufferSize: Int)(implicit n: Numeric[T]) extends Iterable[T] with ExternalMergeSort with ReversableSortedIterable[T] {

  private lazy val sortedFile = sort(it, directory)

  implicit private val b2n = byteArrayToNumber[T] _

  def iterator: Iterator[T] = inputStreamIterator[T](new FileInputStream(sortedFile))

  def reverse: SortedIterable[T] = new SortedIterable[T] {
    def iterator: Iterator[T] = inputStreamIterator[T](new ReversedFileInputStream(sortedFile, n.zero.length))
  }

}

object SortedPersistentNumericIterable {

  def apply[A, B](it: Iterator[A], directory: File, bufferSize: Int)(f: ReversableSortedIterable[A] => B)(implicit n: Numeric[A]): B = {
    val spni = new SortedPersistentNumericIterable(it, directory, bufferSize)
    try {
      f(spni)
    } finally {
      spni.sortedFile.delete()
    }
  }

  def apply[A, B](it: SortedIterable[A], file: File)(f: ReversableSortedIterable[A] => B)(implicit n: Numeric[A]): B = PersistentNumericIterable(it, file) { it =>
    implicit val b2n = byteArrayToNumber[A] _
    val _it = it.iterator
    val rsi = new ReversableSortedIterable[A] {
      def reverse: SortedIterable[A] = {
        while (_it.hasNext) _it.next()
        new SortedIterable[A] {
          def iterator: Iterator[A] = inputStreamIterator[A](new ReversedFileInputStream(file, n.zero.length))
        }
      }

      def iterator: Iterator[A] = if (_it.hasNext) _it else it.iterator
    }
    f(rsi)
  }

}
