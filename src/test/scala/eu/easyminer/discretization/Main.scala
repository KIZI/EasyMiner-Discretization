package eu.easyminer.discretization

import eu.easyminer.discretization.task.EquisizeDiscretizationTask

import scala.collection.JavaConverters._
import scala.util.Random

/**
  * Created by propan on 1. 4. 2017.
  */
object Main extends App {

  val it = new java.lang.Iterable[java.lang.Double] {
    def iterator(): java.util.Iterator[java.lang.Double] = Iterator.fill(1000)(Random.nextInt(1000)).map(x => java.lang.Double.valueOf(x)).asJava
  }

  val r = DefaultDiscretization.getInstance().discretize(new EquisizeDiscretizationTask {
    def getMinSupport: Support = new RelativeSupport(0.2)

    def getBufferSize: Int = 100000
  }, it, classOf[java.lang.Double])

  r.foreach { x =>
    println(s"[${x.getLeftBoundValue},${x.getRightBoundValue}]")
  }

}
