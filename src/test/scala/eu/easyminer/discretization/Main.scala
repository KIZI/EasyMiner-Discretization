package eu.easyminer.discretization

import eu.easyminer.discretization.task.EquisizeDiscretizationTask

import scala.collection.JavaConverters._
import scala.util.Random

/**
  * Created by propan on 1. 4. 2017.
  */
object Main extends App {

  val it = new java.lang.Iterable[Integer] {
    def iterator(): java.util.Iterator[Integer] = Iterator.fill(1000)(Random.nextInt(1000)).map(Integer.valueOf).asJava
  }

  val r = DefaultDiscretization.getInstance().discretize(new EquisizeDiscretizationTask {
    def getMinSupport: Support = new AbsoluteSupport(50)

    def getBufferSize: Int = 100000
  }, it, classOf[Integer])

  r.foreach { x =>
    println(s"[${x.getLeftBoundValue},${x.getRightBoundValue}]")
  }

}
