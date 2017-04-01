package eu.easyminer.discretization.util

import java.io._

/**
  * Created by propan on 19. 3. 2017.
  */
trait PersistentIteratorOps {

  def outputStreamIterator[T](it: Iterator[T], outputStream: OutputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte]): Iterator[T] = new Iterator[T] {
    private lazy val bufferedOutputStream = new BufferedOutputStream(outputStream)

    def hasNext: Boolean = it.hasNext

    def next(): T = try {
      val value = it.next()
      bufferedOutputStream.write(value)
      if (!hasNext) bufferedOutputStream.close()
      value
    } catch {
      case th: Throwable =>
        bufferedOutputStream.close()
        throw th
    }

  }

  def inputStreamIterator[T](inputStream: InputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte], byteArrayToNumeric: Array[Byte] => T): Iterator[T] = new Iterator[T] {
    private lazy val bufferedInputStream = new BufferedInputStream(inputStream)
    private val readBytes: Array[Byte] = n.zero
    private var currentValue = Option.empty[T]
    private var closed = false

    private def hasReadNextNumber = if (bufferedInputStream.read(readBytes) != -1) {
      currentValue = Some(readBytes)
      true
    } else {
      bufferedInputStream.close()
      closed = true
      false
    }

    def hasNext: Boolean = currentValue.nonEmpty || (!closed && hasReadNextNumber)

    def next(): T = if (hasNext) {
      val value = currentValue.get
      currentValue = None
      value
    } else {
      Iterator.empty.next()
    }
  }

}

object PersistentIteratorOps extends PersistentIteratorOps