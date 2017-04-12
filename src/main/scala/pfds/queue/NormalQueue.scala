package pfds.queue

private trait NormalQueue [T] extends Queue[T] {

    private[queue] def front: List[T]
    private[queue] def rear: List[T]

    def snoc[R >: T](x: R): Queue[R] = {
      if (front.isEmpty) NormalQueue(List(x), Nil)
      else NormalQueue(front, x::rear)
    }

    def tail: Queue[T] = NormalQueue(front.tail, rear)

    def isEmpty: Boolean = front.isEmpty

    def head: T = front.head
}

object NormalQueue {

  def apply[T](xs: T*): Queue[T] = {
    new NormalQueue[T] {
      private[queue] val rear: List[T] = Nil
      private[queue] val front: List[T] = xs.toList
    }
  }

  private def apply[T](f: List[T], r: List[T]): Queue[T] = {
    val (f1, r1) =
      if (f.isEmpty) (r.reverse, Nil)
      else (f, r)
    new NormalQueue[T] {
      private[queue] val front: List[T] = f1
      private[queue] val rear: List[T] = r1
    }
  }

  val empty: Queue[Nothing] = NormalQueue()
}
