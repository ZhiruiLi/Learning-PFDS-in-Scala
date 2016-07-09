package pfds.queue


trait Queue[+T] {

  def isEmpty: Boolean

  // cons on the right
  def snoc[R >: T](x: R): Queue[R]
  def head: T
  def tail: Queue[T]

}

package normal {

  private[normal] trait QueueImpl[T] extends Queue[T] {

    private[normal] def front: List[T]
    private[normal] def rear: List[T]

    def snoc[R >: T](x: R): Queue[R] = {
      if (front.isEmpty) Queue(List(x), Nil)
      else Queue(front, x::rear)
    }

    def tail: Queue[T] = Queue(front.tail, rear)

    def isEmpty: Boolean = front.isEmpty

    def head: T = front.head
  }

  object Queue {

    def apply[T](xs: T*): Queue[T] = {
      new QueueImpl[T] {
        private[normal] val rear: List[T] = Nil
        private[normal] val front: List[T] = xs.toList
      }
    }

    private[normal] def apply[T](f: List[T], r: List[T]): Queue[T] = {
      val (f1, r1) =
        if (f.isEmpty) (r.reverse, Nil)
        else (f, r)
      new QueueImpl[T] {
        private[normal] val front: List[T] = f1
        private[normal] val rear: List[T] = r1
      }
    }

    val empty: Queue[Nothing] = Queue()
  }
}


package banker {

  private[banker] trait QueueImpl[T] extends Queue[T] {

    private[banker] def front: Stream[T]
    private[banker] def rear: Stream[T]
    private[banker] def frontLen: Int
    private[banker] def rearLen: Int

    def snoc[R >: T](x: R): Queue[R] = {
      Queue(front, frontLen, Stream.cons(x, rear), rearLen + 1)
    }

    def tail: Queue[T] = Queue(front.tail, frontLen - 1, rear, rearLen)

    def isEmpty: Boolean = frontLen == 0

    def head: T = front.head
  }

  object Queue {

    def apply[T](xs: T*): Queue[T] = {
      new QueueImpl[T] {
        private[banker] val front: Stream[T] = xs.toStream
        private[banker] val rear: Stream[T] = Stream.empty
        private[banker] val frontLen: Int = xs.length
        private[banker] val rearLen: Int = 0
      }
    }

    private[banker] def apply[T](f: Stream[T], fLen: Int,
                                 r: Stream[T], rLen: Int): Queue[T] = {
      val (f1, fLen1, r1, rLen1) =
        if (rLen <= fLen) (f, fLen, r, rLen)
        else (f ++ r.reverse, fLen + rLen, Stream.empty, 0)
      new QueueImpl[T] {
        private[banker] val front: Stream[T] = f1
        private[banker] val rear: Stream[T] = r1
        private[banker] val frontLen: Int = fLen1
        private[banker] val rearLen: Int = rLen1
      }
    }

    val empty: Queue[Nothing] = Queue()
  }
}


package physicist {

  private[physicist] trait QueueImpl[T] extends Queue[T] {

    private[physicist] def front: List[T]
    private[physicist] def rear: List[T]
    private[physicist] def workingCopy: List[T]
    private[physicist] def frontLen: Int
    private[physicist] def rearLen: Int

    def snoc[R >: T](x: R): Queue[R] = {
      Queue(workingCopy, front, frontLen, (x::rear), rearLen + 1)
    }

    def tail: Queue[T] = {
      Queue(workingCopy.tail, front.tail, frontLen - 1, rear, rearLen)
    }

    def isEmpty: Boolean = frontLen == 0

    def head: T = workingCopy.head

    private[physicist] def checkWorkingCopy: QueueImpl[T] = {
      if (workingCopy.isEmpty) {
        val self = this
        new QueueImpl[T] {
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

    private[physicist] def checkRear: QueueImpl[T] = {
      if (rearLen <= frontLen) {
        this
      } else {
        val self = this
        new QueueImpl[T] {
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

    def apply[T](xs: T*): Queue[T] = {
      val lst = xs.toList
      Queue(lst, lst, xs.length, Nil, 0)
    }

    private[physicist] def apply[T](copy: List[T],
                                    f: => List[T], fLen: Int,
                                    r: List[T], rLen: Int): Queue[T] = {
      new QueueImpl[T] {
        private[physicist] val workingCopy: List[T] = copy
        private[physicist] lazy val front: List[T] = f
        private[physicist] def frontLen: Int = fLen
        private[physicist] val rear: List[T] = r
        private[physicist] val rearLen: Int = rLen
      }.
        checkRear.
        checkWorkingCopy
    }

    val empty: Queue[Nothing] = Queue()
  }
}

