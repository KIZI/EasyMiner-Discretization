package eu.easyminer.discretization.util

/**
  * Created by propan on 20. 3. 2017.
  */
trait HowLong {

  def howLong[T](message: String)(f: => T): T = {
    val time = System.nanoTime()
    val x = f
    println(message + ": " + (System.nanoTime() - time))
    x
  }

}

object HowLong extends HowLong
