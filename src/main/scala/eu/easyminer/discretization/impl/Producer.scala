package eu.easyminer.discretization.impl

import eu.easyminer.discretization.Consumer
import eu.easyminer.discretization.impl.Producer.KnownSizeProducer

trait Producer[T] extends eu.easyminer.discretization.Producer[T] {
  self =>

  def knownSize: Int = -1

  def size: Int = {
    if (knownSize >= 0) {
      knownSize
    } else {
      var i = 0
      foreach(_ => i += 1)
      i
    }
  }

  def foreach(f: T => Unit): Unit = produce((x: T) => f(x))

  def flatMap[A](g: T => Producer[A]): Producer[A] = (consumer: Consumer[A]) => self.foreach(g(_).foreach(consumer.consume))

  def map[A](g: T => A): Producer[A] = {
    val mapped = new Producer[A] {
      def produce(consumer: Consumer[A]): Unit = self.foreach(x => consumer.consume(g(x)))
    }
    if (knownSize >= 0) new KnownSizeProducer(knownSize, mapped) else mapped
  }

  def reduceOption(f: (T, T) => T): Option[T] = {
    var res = Option.empty[T]
    foreach { x =>
      res = res.map(f(_, x)).orElse(Some(x))
    }
    res
  }

  def take(n: Int): Producer[T] = {
    if (n >= 0) {
      val col = new Producer[T] {
        def produce(consumer: Consumer[T]): Unit = {
          var i = 0
          self.foreach { x =>
            i += 1
            if (i <= n) consumer.consume(x)
            if (i == n) return
          }
        }
      }
      if (knownSize >= 0) {
        new KnownSizeProducer(math.max(math.min(knownSize, n), 0), col)
      } else {
        col
      }
    } else {
      this
    }
  }

  def drop(n: Int): Producer[T] = {
    if (n >= 0) {
      val col = new Producer[T] {
        def produce(consumer: Consumer[T]): Unit = {
          var i = 0
          self.foreach { x =>
            i += 1
            if (i > n) consumer.consume(x)
          }
        }
      }
      if (knownSize >= 0) {
        new KnownSizeProducer(math.max(0, knownSize - n), col)
      } else {
        col
      }
    } else {
      this
    }
  }

  def slice(from: Int, until: Int): Producer[T] = drop(from).take(until - from)

  def foldLeft[A](a: A)(f: (A, T) => A): A = {
    var res = a
    self.foreach(x => res = f(res, x))
    res
  }

}

object Producer {

  private class KnownSizeProducer[T](override val knownSize: Int, producer: Producer[T]) extends Producer[T] {
    def produce(consumer: Consumer[T]): Unit = producer.produce(consumer)
  }

  def apply[T](col: Iterable[T]): Producer[T] = {
    val producer = new Producer[T] {
      def produce(consumer: Consumer[T]): Unit = col.foreach(consumer.consume)
    }
    if (col.knownSize >= 0) new KnownSizeProducer(col.knownSize, producer) else producer
  }

  def apply[T](col: eu.easyminer.discretization.Producer[T]): Producer[T] = (consumer: Consumer[T]) => col.produce(consumer)

}