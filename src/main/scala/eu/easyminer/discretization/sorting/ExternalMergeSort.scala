package eu.easyminer.discretization.sorting

import java.io._

import eu.easyminer.discretization.util.NumericByteArray._

import scala.collection.mutable
import scala.util.Random


/**
  * Created by propan on 19. 3. 2017.
  */
trait ExternalMergeSort {

  val bufferSize: Int

  implicit private class PimpedFile(directory: File) {
    def newChildFile = {
      val file = Stream.continually(new File(directory, Random.alphanumeric.take(8).mkString)).find(!_.exists()).get
      file.createNewFile()
      file
    }
  }

  private class ChunkInfo(val numericBytes: Array[Byte], val chunkSize: Int) {
    val chunkSizeBytes = chunkSize * numericBytes.length
    val halfChunkSize = chunkSize / 2
    val halfChunkSizeBytes = halfChunkSize * numericBytes.length
  }

  private class ChunkSorting[T](chunkInfo: ChunkInfo)(implicit n: Numeric[T]) {

    //it sorts chunks and save them into the file
    //it return total number of saved chunks
    def sortChunks(it: Iterator[T], file: File): Int = {
      val stream = new BufferedOutputStream(new FileOutputStream(file))
      try {
        val buffer = new mutable.PriorityQueue[T]()(n.reverse)
        var numberOfSavedChunks = 0
        def saveBuffer() = {
          if (buffer.nonEmpty) numberOfSavedChunks += 1
          while (buffer.nonEmpty) stream.write(buffer.dequeue())
        }
        for (x <- it) {
          buffer.enqueue(x)
          if (buffer.size == chunkInfo.chunkSize) {
            saveBuffer()
          }
        }
        saveBuffer()
        numberOfSavedChunks
      } finally {
        stream.close()
      }
    }

  }

  private class ChunkMerging[T](chunkInfo: ChunkInfo, numberOfSavedChunks: Int)(implicit n: Numeric[T]) {

    //it merges two chunks in memory which has size chunk1.next.size + chunk2.next.size
    //it returns one sorted iterator from two sorted chunks
    def mergeTwoChunks(chunk1: Iterator[Iterator[T]], chunk2: Iterator[Iterator[T]]): Iterator[T] = new Iterator[T] {
      var chunkList1 = if (chunk1.hasNext) chunk1.next() else Iterator.empty
      var chunkList2 = if (chunk2.hasNext) chunk2.next() else Iterator.empty
      var head1 = Option.empty[T]
      var head2 = Option.empty[T]

      def compare(x1: Option[T], x2: Option[T]): Either[T, T] = if (x1.isEmpty) {
        Right(x2.get)
      } else if (x2.isEmpty) {
        Left(x1.get)
      } else if (n.lteq(x1.get, x2.get)) {
        Left(x1.get)
      } else {
        Right(x2.get)
      }

      def loadChunk(chunk: Iterator[T], it: Iterator[Iterator[T]]) = if (chunk.isEmpty) {
        if (it.hasNext) it.next() else Iterator.empty
      } else {
        chunk
      }

      def loadHead(head: Option[T], it: Iterator[T]): Option[T] = head.orElse(if (it.hasNext) Some(it.next()) else None)

      def hasNext: Boolean = chunkList1.hasNext || chunkList2.hasNext || head1.nonEmpty || head2.nonEmpty

      def next(): T = if (hasNext) {
        head1 = loadHead(head1, chunkList1)
        head2 = loadHead(head2, chunkList2)
        compare(head1, head2) match {
          case Left(x) =>
            chunkList1 = loadChunk(chunkList1, chunk1)
            head1 = None
            x
          case Right(x) =>
            chunkList2 = loadChunk(chunkList2, chunk2)
            head2 = None
            x
        }
      } else {
        Iterator.empty.next()
      }
    }

    def loadTwoChunks(raf: RandomAccessFile)(chunkOffset: Int, adjacentSortedChunks: Int): (Iterator[Iterator[T]], Iterator[Iterator[T]]) = {
      val offset1 = chunkOffset.toLong * chunkInfo.chunkSizeBytes
      val offset2 = offset1 + chunkInfo.chunkSizeBytes * adjacentSortedChunks
      def createIterator(offset: Long, endOffset: Long): Iterator[Iterator[T]] = new Iterator[Iterator[T]] {
        var _offset = offset

        def readIntoBuffer: Iterator[T] = {
          val buffer = new Array[Byte](chunkInfo.halfChunkSizeBytes)
          val readBytes = raf.read(buffer)
          if (readBytes == -1) {
            Iterator.empty
          } else {
            _offset += readBytes
            buffer.iterator.take(readBytes).grouped(chunkInfo.numericBytes.length).map(x => Array(x: _*): T)
          }
        }

        def hasNext: Boolean = _offset < endOffset

        def next(): Iterator[T] = if (hasNext) {
          raf.seek(_offset)
          readIntoBuffer
        } else {
          Iterator.empty.next()
        }
      }
      createIterator(offset1, offset2 - 1) -> createIterator(offset2, offset2 + chunkInfo.chunkSizeBytes * adjacentSortedChunks - 1)
    }

    def mergeSortedChunks(file: File): File = {
      @scala.annotation.tailrec
      def mergeSortedChunksWithLength(file: File, adjacentSortedChunks: Int): File = if (adjacentSortedChunks >= numberOfSavedChunks) {
        file
      } else {
        val raf = new RandomAccessFile(file, "r")
        val loadTwoChunksRaf = loadTwoChunks(raf) _
        val desiredAdjacentSortedChunks = adjacentSortedChunks * 2
        val newFile = file.getParentFile.newChildFile
        val stream = new BufferedOutputStream(new FileOutputStream(newFile))
        try {
          for (chunkOffset <- 0 until numberOfSavedChunks by desiredAdjacentSortedChunks) {
            val (it1, it2) = loadTwoChunksRaf(chunkOffset, adjacentSortedChunks)
            mergeTwoChunks(it1, it2).foreach(x => stream.write(x))
          }
        } finally {
          stream.close()
          raf.close()
          file.delete()
        }
        mergeSortedChunksWithLength(newFile, desiredAdjacentSortedChunks)
      }
      mergeSortedChunksWithLength(file, 1)
    }

  }

  def sort[T](it: Iterator[T], directory: File)(implicit n: Numeric[T]): File = {
    val chunkInfo = {
      val numericBytes: Array[Byte] = n.zero
      val chunkSize = {
        val chunkSize = math.floor(bufferSize.toDouble / numericBytes.length).toInt
        if (chunkSize % 2 == 0) chunkSize else chunkSize + 1
      }
      new ChunkInfo(n.zero, chunkSize)
    }
    val sortedChunksFile = directory.newChildFile
    val numberOfSavedChunks = new ChunkSorting[T](chunkInfo).sortChunks(it, sortedChunksFile)
    new ChunkMerging[T](chunkInfo, numberOfSavedChunks).mergeSortedChunks(sortedChunksFile)
  }

}
