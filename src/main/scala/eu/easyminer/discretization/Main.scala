package eu.easyminer.discretization

import java.io.File
import java.nio.ByteBuffer

import eu.easyminer.discretization.sorting.SortedPersistentNumericIterable
import eu.easyminer.discretization.util.ReversedFileInputStream

import scala.collection.mutable
import scala.util.Random

/**
  * Created by propan on 16. 3. 2017.
  */
object Main extends App {

  val a = new ReversedFileInputStream(new File("5aeqOep9"), 4)

  val b = new Array[Byte](80)
  val x = mutable.MutableList.empty[Byte]
    Stream.continually(a.read(b)).takeWhile(x => x != -1).foreach { i =>
    println("read bytes: " + i)
    x ++= b.take(i)
  }

  x.iterator.grouped(4).foreach{ x =>
    println(ByteBuffer.wrap(x.toArray).getInt)
  }

}
