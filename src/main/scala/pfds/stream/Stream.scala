package pfds.stream

sealed trait Stream[+T] {

  def head: T
  def tail: Stream[T]

  def ::[R >: T](v: => R): Stream[R] = {
    Cons(v, this)
  }

  def take(n: Int): Stream[T] = (n, this) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (_, Cons(v, tail)) => Cons(v, take(n - 1))
  }

  def drop(n: Int): Stream[T] = (n, this) match {
    case (0, _) => this
    case (_, Nil) => Nil
    case (_, _) => tail.drop(n - 1)
  }

  def append[R >: T](s: Stream[R]): Stream[R] = this match {
    case Nil => s
    case Cons(head, tail) => Cons(head, tail.append(s))
  }

  private def reverseRec[R >: T](s: Stream[R]): Stream[R] = this match {
    case Nil => s
    case Cons(_, _) => tail.reverseRec(Cons(head, s))
  }

  def reverse: Stream[T] = reverseRec(Nil)
}

object Nil extends Stream[Nothing] {

  def head: Nothing = throw new RuntimeException("Empty Stream")
  def tail: Stream[Nothing] = throw new RuntimeException("Empty Stream")
}

class Cons[+T](v: => T, s: => Stream[T]) extends Stream[T] {

  lazy val head: T = v
  lazy val tail: Stream[T] = s
}

object Cons {

  def apply[T](v: => T, s: => Stream[T]): Stream[T] = {
    new Cons(v, s)
  }

  def unapply[T](stream: Stream[T]): Option[(T, Stream[T])] = stream match {
    case x: Cons[T] => Some((x.head, x.tail))
    case _          => None
  }
}

object ::{

  def unapply[T](stream: Stream[T]): Option[(T, Stream[T])] = stream match {
    case x: Cons[T] => Some((x.head, x.tail))
    case _          => None
  }
}

