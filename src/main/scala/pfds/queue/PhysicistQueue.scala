package pfds.queue

private[queue] trait PhysicistQueue[T] extends Queue[T] {

  private[queue] def front: List[T]
  private[queue] def rear: List[T]
  private[queue] def workingCopy: List[T]
  private[queue] def frontLen: Int
  private[queue] def rearLen: Int

  def snoc[R >: T](x: R): Queue[R] = {
    PhysicistQueue(workingCopy, front, frontLen, (x::rear), rearLen + 1)
  }

  def tail: Queue[T] = {
    PhysicistQueue(workingCopy.tail, front.tail, frontLen - 1, rear, rearLen)
  }

  def isEmpty: Boolean = frontLen == 0

  def head: T = workingCopy.head

  private[queue] def checkWorkingCopy: PhysicistQueue[T] = {
    if (workingCopy.isEmpty) {
      val self = this
      new PhysicistQueue[T] {
        private[queue] val workingCopy: List[T] = self.front
        private[queue] val front: List[T] = self.front
        private[queue] val frontLen: Int = self.frontLen
        private[queue] val rear: List[T] = self.rear
        private[queue] val rearLen: Int = self.rearLen
      }
    } else {
      this
    }
  }

  private[queue] def checkRear: PhysicistQueue[T] = {
    if (rearLen <= frontLen) {
      this
    } else {
      val self = this
      new PhysicistQueue[T] {
        private[queue] val workingCopy: List[T] = self.front
        private[queue] lazy val front: List[T] = self.front ++ self.rear.reverse
        private[queue] val frontLen: Int = self.frontLen + self.rearLen
        private[queue] val rear: List[T] = Nil
        private[queue] val rearLen: Int = 0
      }
    }
  }
}

object PhysicistQueue {

  def apply[T](xs: T*): Queue[T] = {
    val lst = xs.toList
    PhysicistQueue(lst, lst, xs.length, Nil, 0)
  }

  private def apply[T](copy: List[T],
                       f: => List[T], fLen: Int,
                       r: List[T], rLen: Int): Queue[T] = {
    new PhysicistQueue[T] {
      private[queue] val workingCopy: List[T] = copy
      private[queue] lazy val front: List[T] = f
      private[queue] def frontLen: Int = fLen
      private[queue] val rear: List[T] = r
      private[queue] val rearLen: Int = rLen
    }.
      checkRear.
      checkWorkingCopy
  }

  val empty: Queue[Nothing] = PhysicistQueue()
}

