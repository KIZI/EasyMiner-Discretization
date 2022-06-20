package eu.easyminer.discretization.util

import eu.easyminer.discretization.Consumer
import eu.easyminer.discretization.impl.Producer

import java.io._

/**
 * Created by propan on 19. 3. 2017.
 */
trait PersistentProducerOps {

  def outputStreamProducer[T](col: Producer[T], outputStream: => OutputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte]): Producer[T] = new Producer[T] {
    override def knownSize: Int = col.knownSize

    def produce(consumer: Consumer[T]): Unit = {
      val bufferedOutputStream = new BufferedOutputStream(outputStream)
      try {
        for (x <- col) {
          bufferedOutputStream.write(x)
          consumer.consume(x)
        }
      } finally {
        bufferedOutputStream.close()
      }
    }
  }

  def inputStreamProducer[T](inputStream: => InputStream)(implicit n: Numeric[T], numericToByteArray: T => Array[Byte], byteArrayToNumeric: Array[Byte] => T): Producer[T] = (consumer: Consumer[T]) => {
    val bufferedInputStream = new BufferedInputStream(inputStream)
    val readBytes: Array[Byte] = n.zero
    try {
      Iterator.continually(bufferedInputStream.read(readBytes)).takeWhile(_ != -1).map(_ => byteArrayToNumeric(readBytes)).foreach(consumer.consume)
    } finally {
      bufferedInputStream.close()
    }
  }

}

object PersistentProducerOps extends PersistentProducerOps