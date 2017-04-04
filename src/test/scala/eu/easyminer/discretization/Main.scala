package eu.easyminer.discretization

import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask}
import eu.easyminer.discretization.util.HowLong._

import scala.collection.JavaConverters._
import scala.util.Random

/**
  * Created by propan on 1. 4. 2017.
  */
object Main extends App {

  val it = new java.lang.Iterable[java.lang.Double] {
    def iterator(): java.util.Iterator[java.lang.Double] = Iterator.fill(1000000)(Random.nextInt(100)).map(x => java.lang.Double.valueOf(x)).asJava
  }

  val r1 = howLong("equisize")(DefaultDiscretization.getInstance().discretize(new EquisizeDiscretizationTask {
    def getMinSupport: Support = new RelativeSupport(0.2)

    def getBufferSize: Int = 10000000
  }, it, classOf[java.lang.Double]))

  val r2 = howLong("equifrequency")(DefaultDiscretization.getInstance().discretize(new EquifrequencyDiscretizationTask {
    def getNumberOfBins: Int = 10

    def getBufferSize: Int = 10000000
  }, it, classOf[java.lang.Double]))

  val r3 = howLong("equidistance")(DefaultDiscretization.getInstance().discretize(new EquidistanceDiscretizationTask {
    def getNumberOfBins: Int = 10

    def getBufferSize: Int = 10000000
  }, it, classOf[java.lang.Double]))

  Iterator(r1, r2, r3).foreach { r =>
    println("**********************")
    r.foreach { x =>
      println(s"[${x.getLeftBoundValue},${x.getRightBoundValue}]")
    }
  }

}
