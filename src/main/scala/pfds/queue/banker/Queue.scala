package pfds.queue.banker

private[banker] trait Queue[T] extends pfds.queue.Queue[T] {

  private[banker] def front: Stream[T]
  private[banker] def rear: Stream[T]
  private[banker] def frontLen: Int
  private[banker] def rearLen: Int

  def snoc[R >: T](x: R): pfds.queue.Queue[R] = {
    Queue(front, frontLen, Stream.cons(x, rear), rearLen + 1)
  }

  def tail: pfds.queue.Queue[T] =
    Queue(front.tail, frontLen - 1, rear, rearLen)

  def isEmpty: Boolean = frontLen == 0

  def head: T = front.head
}

object Queue {

  def apply[T](xs: T*): pfds.queue.Queue[T] = {
    new Queue[T] {
      private[banker] val front: Stream[T] = xs.toStream
      private[banker] val rear: Stream[T] = Stream.empty
      private[banker] val frontLen: Int = xs.length
      private[banker] val rearLen: Int = 0
    }
  }

  private def apply[T](f: Stream[T], fLen: Int,
                       r: Stream[T], rLen: Int): pfds.queue.Queue[T] = {
    val (f1, fLen1, r1, rLen1) =
      if (rLen <= fLen) (f, fLen, r, rLen)
      else (f ++ r.reverse, fLen + rLen, Stream.empty, 0)
    new Queue[T] {
      private[banker] val front: Stream[T] = f1
      private[banker] val rear: Stream[T] = r1
      private[banker] val frontLen: Int = fLen1
      private[banker] val rearLen: Int = rLen1
    }
  }

  val empty: pfds.queue.Queue[Nothing] = Queue()
}

