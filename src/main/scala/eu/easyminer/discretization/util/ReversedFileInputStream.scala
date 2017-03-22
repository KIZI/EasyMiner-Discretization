package eu.easyminer.discretization.util

import java.io.{File, IOException, InputStream, RandomAccessFile}

/**
  * Created by propan on 21. 3. 2017.
  */
class ReversedFileInputStream(file: File, groupSize: Int = 1) extends InputStream {

  if (groupSize < 1) throw new IllegalArgumentException

  private val raf = new RandomAccessFile(file, "r")

  private var overflow: List[Byte] = Nil

  private var offset = {
    val offset = file.length()
    raf.seek(offset)
    offset
  }

  private def readBytes(count: Int) = {
    val limit = {
      val limit = count + (groupSize - (count % groupSize))
      val _offset = offset - limit
      if (_offset < 0) {
        offset = 0
        limit + _offset.toInt
      } else {
        offset = _offset
        limit
      }
    }
    if (limit > 0) {
      if (limit % groupSize != 0) throw new IOException(s"Rest of stream does not fit on the group size: $groupSize bytes.")
      val a = new Array[Byte](limit)
      raf.seek(offset)
      raf.read(a)
      a
    } else {
      Array.emptyByteArray
    }
  }

  def read(): Int = {
    val a = new Array[Byte](1)
    if (read(a) == 1) {
      a(0) & 0xFF
    } else {
      -1
    }
  }

  override def close(): Unit = raf.close()

  override def read(b: Array[Byte]): Int = read(b, 0, b.length)

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    val it = (overflow.iterator ++ readBytes(len).grouped(groupSize).toList.reverseIterator.flatten).grouped(len)
    if (it.isEmpty) {
      -1
    } else {
      val a = it.next()
      for ((x, i) <- a.iterator.zipWithIndex) b.update(i + off, x)
      overflow = it.flatten.toList
      a.length
    }
  }

}