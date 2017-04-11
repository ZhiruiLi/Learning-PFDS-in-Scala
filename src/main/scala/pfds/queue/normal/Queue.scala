package pfds.queue.normal

private[normal] trait Queue[T] extends pfds.queue.Queue[T] {

  private[normal] def front: List[T]
  private[normal] def rear: List[T]

  def snoc[R >: T](x: R): pfds.queue.Queue[R] = {
    if (front.isEmpty) Queue(List(x), Nil)
    else Queue(front, x::rear)
  }

  def tail: pfds.queue.Queue[T] = Queue(front.tail, rear)

  def isEmpty: Boolean = front.isEmpty

  def head: T = front.head
}

object Queue {

  def apply[T](xs: T*): pfds.queue.Queue[T] = {
    new Queue[T] {
      private[normal] val rear: List[T] = Nil
      private[normal] val front: List[T] = xs.toList
    }
  }

  private def apply[T](f: List[T], r: List[T]): pfds.queue.Queue[T] = {
    val (f1, r1) =
      if (f.isEmpty) (r.reverse, Nil)
      else (f, r)
    new Queue[T] {
      private[normal] val front: List[T] = f1
      private[normal] val rear: List[T] = r1
    }
  }

  val empty: pfds.queue.Queue[Nothing] = Queue()
}
