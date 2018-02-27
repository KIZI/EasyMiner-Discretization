package eu.easyminer.discretization.impl.sorting

import java.io.{File, FileInputStream}

import eu.easyminer.discretization.impl.PersistentNumericTraversable
import eu.easyminer.discretization.util.NumericByteArray._
import eu.easyminer.discretization.util.PersistentTraversableOps._
import eu.easyminer.discretization.util.ReversedFileInputStream

/**
  * Created by propan on 19. 3. 2017.
  */
object SortedPersistentNumericTraversable {

  def apply[A, B](col: Traversable[A], directory: File, bufferSize: Int)(f: ReversableSortedTraversable[A] => B)(implicit n: Numeric[A]): B = {
    implicit val b2n: Array[Byte] => A = byteArrayToNumber[A]
    val ems = new ExternalMergeSort(bufferSize)
    lazy val sortedFile = ems.sort(col, directory)
    try {
      val rst = new ReversableSortedTraversable[A](
        new Traversable[A] {
          def foreach[U](f: A => U): Unit = inputStreamTraversable[A](new FileInputStream(sortedFile)).foreach(f)
        },
        new Traversable[A] {
          def foreach[U](f: A => U): Unit = inputStreamTraversable[A](new ReversedFileInputStream(sortedFile, n.zero.length)).foreach(f)
        }
      )
      f(rst)
    } finally {
      sortedFile.delete()
    }
  }

  def apply[A, B](col: SortedTraversable[A], file: File)(f: ReversableSortedTraversable[A] => B)(implicit n: Numeric[A]): B = PersistentNumericTraversable(col, file) { col =>
    implicit val b2n: Array[Byte] => A = byteArrayToNumber[A]
    val rst = new ReversableSortedTraversable[A](
      col,
      new Traversable[A] {
        def foreach[U](f: A => U): Unit = {
          if (!file.exists()) col.foreach(_ => Unit)
          inputStreamTraversable[A](new ReversedFileInputStream(file, n.zero.length)).foreach(f)
        }
      }
    )
    f(rst)
  }

}
