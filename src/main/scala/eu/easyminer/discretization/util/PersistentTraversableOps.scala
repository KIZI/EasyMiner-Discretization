package eu.easyminer.discretization.util

import java.io._

/**
  * Created by propan on 19. 3. 2017.
  */
trait PersistentTraversableOps {

  def outputStreamTraversable[T](col: Traversable[T], outputStream: OutputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte]): Traversable[T] = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      val bufferedOutputStream = new BufferedOutputStream(outputStream)
      try {
        for (value <- col) {
          bufferedOutputStream.write(value)
          f(value)
        }
      } finally {
        bufferedOutputStream.close()
      }
    }
  }

  def inputStreamTraversable[T](inputStream: InputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte], byteArrayToNumeric: Array[Byte] => T): Traversable[T] = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      val bufferedInputStream = new BufferedInputStream(inputStream)
      val readBytes: Array[Byte] = n.zero
      try {
        Stream.continually(bufferedInputStream.read(readBytes)).takeWhile(_ != -1).foreach(_ => f(readBytes))
      } finally {
        bufferedInputStream.close()
      }
    }
  }

}

object PersistentTraversableOps extends PersistentTraversableOps