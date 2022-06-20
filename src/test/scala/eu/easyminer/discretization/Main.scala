package eu.easyminer.discretization

import eu.easyminer.discretization.task.{EquidistanceDiscretizationTask, EquifrequencyDiscretizationTask, EquisizeDiscretizationTask, EquisizeTreeDiscretizationTask}
import eu.easyminer.discretization.util.HowLong._

import java.lang
import scala.util.Random

/**
 * Created by propan on 1. 4. 2017.
 */
object Main {

  def main(args: Array[String]): Unit = {
    val it = new Producer[java.lang.Double] {
      def produce(consumer: Consumer[lang.Double]): Unit = Iterator.fill(1000000)(Random.nextInt(100)).map(x => java.lang.Double.valueOf(x)).foreach(consumer.consume)
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

    val r4 = howLong("equisizetree")(DefaultDiscretization.getInstance().discretize(new EquisizeTreeDiscretizationTask {
      def getMinSupport: Support = new RelativeSupport(0.05)

      def getArity: Int = 2

      def inParallel(): Boolean = true

      def getBufferSize: Int = 10000000
    }, it, classOf[java.lang.Double]))

    Iterator(r1, r2, r3, r4).foreach { r =>
      println("**********************")
      r.foreach { x =>
        println(s"[${x.getLeftBoundValue},${x.getRightBoundValue}]")
      }
    }
  }

}
