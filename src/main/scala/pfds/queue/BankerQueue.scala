package pfds.queue

private[queue] trait BankerQueue[T] extends Queue[T] {

  private[queue] def front: Stream[T]
  private[queue] def rear: Stream[T]
  private[queue] def frontLen: Int
  private[queue] def rearLen: Int

  def snoc[R >: T](x: R): Queue[R] = {
    BankerQueue(front, frontLen, Stream.cons(x, rear), rearLen + 1)
  }

  def tail: Queue[T] = BankerQueue(front.tail, frontLen - 1, rear, rearLen)

  def isEmpty: Boolean = frontLen == 0

  def head: T = front.head
}

object BankerQueue {

  def apply[T](xs: T*): Queue[T] = {
    new BankerQueue[T] {
      private[queue] val front: Stream[T] = xs.toStream
      private[queue] val rear: Stream[T] = Stream.empty
      private[queue] val frontLen: Int = xs.length
      private[queue] val rearLen: Int = 0
    }
  }

  private def apply[T](f: Stream[T], fLen: Int,
                       r: Stream[T], rLen: Int): Queue[T] = {
    val (f1, fLen1, r1, rLen1) =
      if (rLen <= fLen) (f, fLen, r, rLen)
      else (f ++ r.reverse, fLen + rLen, Stream.empty, 0)
    new BankerQueue[T] {
      private[queue] val front: Stream[T] = f1
      private[queue] val rear: Stream[T] = r1
      private[queue] val frontLen: Int = fLen1
      private[queue] val rearLen: Int = rLen1
    }
  }

  val empty: Queue[Nothing] = BankerQueue()
}

