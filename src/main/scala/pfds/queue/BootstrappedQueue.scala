package pfds.queue

import bootstrapped._

package bootstrapped {
  class Lazy[+T](value: => T) {
    lazy val get: T = value
  }
  object Lazy {
    def apply[T](value: => T) = new Lazy(value)
  }
}

sealed trait BootstrappedQueue[+T] extends Queue[T] {

  override def snoc[R >: T](x: R): BootstrappedQueue[R]

  override def tail: BootstrappedQueue[T]
}

case object BootstrappedQueueEmpty extends BootstrappedQueue[Nothing] {

  val isEmpty: Boolean = true

  def snoc[R >: Nothing](x: R): BootstrappedQueue[R] =
    BootstrappedQueueNonEmpty(List(x), BootstrappedQueueEmpty, 1, Nil, 0)

  def head: Nothing = throw new IndexOutOfBoundsException

  def tail: BootstrappedQueue[Nothing] = throw new IndexOutOfBoundsException
}

case class BootstrappedQueueNonEmpty[+T](front: List[T],
                                         middle: BootstrappedQueue[Lazy[List[T]]],
                                         lenFM: Int,
                                         rear: List[T],
                                         lenR: Int) extends BootstrappedQueue[T] {

  private def queue[R](front: List[R],
                       middle: BootstrappedQueue[Lazy[List[R]]],
                       lenFM: Int,
                       rear: List[R],
                       lenR: Int): BootstrappedQueue[R] = {
    if (lenR <= lenFM) {
      checkFront(front, middle, lenFM, rear, lenR)
    } else {
      checkFront(front, middle.snoc(Lazy(rear.reverse)), lenFM + lenR, Nil, 0)
    }
  }

  private def checkFront[R](front: List[R],
                            middle: BootstrappedQueue[Lazy[List[R]]],
                            lenFM: Int,
                            rear: List[R],
                            lenR: Int): BootstrappedQueue[R] = (front, middle) match {
    case (Nil, BootstrappedQueueEmpty) =>
      BootstrappedQueueEmpty
    case (Nil, _) =>
      BootstrappedQueueNonEmpty[R](middle.head.get, middle.tail, lenFM, rear, lenR)
    case _ =>
      BootstrappedQueueNonEmpty(front, middle, lenFM, rear, lenR)
  }

  val isEmpty: Boolean = false

  def snoc[R >: T](x: R): BootstrappedQueue[R] = queue(front, middle, lenFM, x::rear, lenR + 1)

  def head: T = front.head

  def tail: BootstrappedQueue[T] = queue(front.tail, middle, lenFM - 1, rear, lenR)
}

object BootstrappedQueue {

  def apply[T](xs: T*): BootstrappedQueue[T] =
    if (xs.isEmpty) BootstrappedQueueEmpty
    else BootstrappedQueueNonEmpty(xs.toList, empty, xs.length, Nil, 0)

  def empty: BootstrappedQueue[Nothing] = BootstrappedQueueEmpty
}
