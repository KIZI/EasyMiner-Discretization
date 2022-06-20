package eu.easyminer.discretization.algorithm

import eu.easyminer.discretization.algorithm.Discretization.Exceptions.IllegalTypeOfTraversable
import eu.easyminer.discretization.impl._
import eu.easyminer.discretization.impl.sorting.SortedProducer
import scala.collection.parallel.CollectionConverters._

/**
  * Created by propan on 31. 3. 2017.
  */
class EquisizedIntervalsTree[T] private[algorithm](minSupport: Support, arity: Int, inParallel: Boolean)(implicit val n: Numeric[T]) extends Discretization[T] {

  private case class IntervalTree(interval: Interval.WithFrequency, children: collection.mutable.ListBuffer[IntervalTree])

  private val equifrequentIntervals = new EquifrequentIntervals[T](arity)

  private def getFirstInterval(data: Producer[T]): Option[IntervalTree] = {
    var minValue = Option.empty[T]
    var maxValue = Option.empty[T]
    var i = 0
    for (x <- data) {
      if (minValue.isEmpty) minValue = Some(x)
      maxValue = Some(x)
      i += 1
    }
    for {
      minValue <- minValue
      maxValue <- maxValue
    } yield {
      IntervalTree(Interval(IntervalBound.Inclusive(n.toDouble(minValue)), IntervalBound.Inclusive(n.toDouble(maxValue)), i), collection.mutable.ListBuffer.empty)
    }
  }

  private def splitIntervals(data: Producer[T], intervals: Seq[IntervalTree], minSupport: Int): Unit = {
    def addIntervals(intervalTree: IntervalTree, intervals: IndexedSeq[Interval.WithFrequency]): Unit = {
      if (intervals.length == arity && intervals.forall(_.frequency >= minSupport)) {
        intervalTree.children ++= intervals.iterator.map(IntervalTree(_, collection.mutable.ListBuffer.empty))
      }
    }

    def discretizeAndAddIntervals(intervalTree: IntervalTree, from: Int, until: Int): Unit = addIntervals(intervalTree, equifrequentIntervals.discretize(new SortedProducer[T](data.slice(from, until))))

    val dataMap = intervals.iterator.map(_ -> collection.mutable.ListBuffer.empty[Int]).toMap
    val it = intervals.iterator.map(x => x -> dataMap(x))
    var currentInterval = Option(it.next())
    var fromIndex = Option.empty[Int]
    var i = 0
    for {
      x <- data
      (intervalTree, range) <- currentInterval
    } {
      if (intervalTree.interval.isInInterval(n.toDouble(x))) {
        if (fromIndex.isEmpty) fromIndex = Some(i)
      } else {
        for (fromKey <- fromIndex) {
          range += (fromKey, i)
          currentInterval = if (it.hasNext) Some(it.next()) else None
          fromIndex = if (currentInterval.exists(_._1.interval.isInInterval(n.toDouble(x)))) Some(i) else None
          if (!inParallel) {
            discretizeAndAddIntervals(intervalTree, range.head, range.last)
          }
        }
      }
      i += 1
    }
    for {
      (intervalTree, range) <- currentInterval
      fromIndex <- fromIndex
    } {
      range += (fromIndex, i)
      if (!inParallel) {
        discretizeAndAddIntervals(intervalTree, range.head, range.last)
      }
    }
    if (inParallel) {
      val res = dataMap.toList.par.map(x => x._1 -> equifrequentIntervals.discretize(new SortedProducer[T](data.slice(x._2.head, x._2.last)))).seq.toMap
      intervals.foreach(x => addIntervals(x, res(x)))
    }
  }

  @scala.annotation.tailrec
  private def expandIntervalsTree(data: Producer[T], intervals: Seq[IntervalTree], minSupport: Int): Unit = {
    splitIntervals(data, intervals, minSupport)
    val allChildren = intervals.iterator.flatMap(_.children).toList
    if (allChildren.nonEmpty) {
      expandIntervalsTree(data, allChildren, minSupport)
    }
  }

  @scala.annotation.tailrec
  private def buildArrayFromTrees(intervals: Seq[IntervalTree], result: collection.mutable.ArrayBuffer[Interval.WithFrequency]): IndexedSeq[Interval.WithFrequency] = {
    result ++= intervals.iterator.map(_.interval)
    if (intervals.exists(_.children.nonEmpty)) {
      val allChilren = intervals.flatMap { intervalTree =>
        if (intervalTree.children.nonEmpty) intervalTree.children else List.fill(arity)(intervalTree)
      }
      buildArrayFromTrees(allChilren, result)
    } else {
      result.toIndexedSeq
    }
  }

  /*
  for ()
    val numberOfIntervals = math.pow(arity, level).toInt
    val from = ((math.pow(arity, level + 1) - 1) / (arity - 1)).toInt
    val until = from + numberOfIntervals
    result.view(from, until)
   */

  def discretize(data: Producer[T]): IndexedSeq[Interval.WithFrequency] = data match {
    case data: SortedProducer[T] =>
      getFirstInterval(data) match {
        case Some(root) =>
          val _minSupport = minSupport match {
            case Support.Relative(minSupport) => math.ceil(root.interval.frequency * minSupport).toInt
            case Support.Absolute(minSupport) => minSupport
          }
          expandIntervalsTree(data, List(root), _minSupport)
          buildArrayFromTrees(List(root), collection.mutable.ArrayBuffer.empty)
        case None => IndexedSeq.empty
      }
    case _ => throw new IllegalTypeOfTraversable(classOf[SortedProducer[T]], data.getClass)
  }

}