package pfds.queue.physicist

private[physicist] trait Queue[T] extends pfds.queue.Queue[T] {

  private[physicist] def front: List[T]
  private[physicist] def rear: List[T]
  private[physicist] def workingCopy: List[T]
  private[physicist] def frontLen: Int
  private[physicist] def rearLen: Int

  def snoc[R >: T](x: R): pfds.queue.Queue[R] = {
    Queue(workingCopy, front, frontLen, (x::rear), rearLen + 1)
  }

  def tail: pfds.queue.Queue[T] = {
    Queue(workingCopy.tail, front.tail, frontLen - 1, rear, rearLen)
  }

  def isEmpty: Boolean = frontLen == 0

  def head: T = workingCopy.head

  private[physicist] def checkWorkingCopy: Queue[T] = {
    if (workingCopy.isEmpty) {
      val self = this
      new Queue[T] {
        private[physicist] val workingCopy: List[T] = self.front
        private[physicist] val front: List[T] = self.front
        private[physicist] val frontLen: Int = self.frontLen
        private[physicist] val rear: List[T] = self.rear
        private[physicist] val rearLen: Int = self.rearLen
      }
    } else {
      this
    }
  }

  private[physicist] def checkRear: Queue[T] = {
    if (rearLen <= frontLen) {
      this
    } else {
      val self = this
      new Queue[T] {
        private[physicist] val workingCopy: List[T] = self.front
        private[physicist] lazy val front: List[T] = self.front ++ self.rear.reverse
        private[physicist] val frontLen: Int = self.frontLen + self.rearLen
        private[physicist] val rear: List[T] = Nil
        private[physicist] val rearLen: Int = 0
      }
    }
  }
}

object Queue {

  def apply[T](xs: T*): pfds.queue.Queue[T] = {
    val lst = xs.toList
    Queue(lst, lst, xs.length, Nil, 0)
  }

  private def apply[T](copy: List[T],
                       f: => List[T], fLen: Int,
                       r: List[T], rLen: Int): pfds.queue.Queue[T] = {
    new Queue[T] {
      private[physicist] val workingCopy: List[T] = copy
      private[physicist] lazy val front: List[T] = f
      private[physicist] def frontLen: Int = fLen
      private[physicist] val rear: List[T] = r
      private[physicist] val rearLen: Int = rLen
    }.
      checkRear.
      checkWorkingCopy
  }

  val empty: pfds.queue.Queue[Nothing] = Queue()
}

