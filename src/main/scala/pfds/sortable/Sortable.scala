package pfds.sortable

trait Sortable[T] {

  def add(element: T): Sortable[T]

  def sort: List[T]
}

object Sortable {

  trait SortableImpl[T] extends Sortable[T] {

    def size: Int

    def segments: List[List[T]]

    def less: (T, T) => Boolean

    def sort: List[T] = {

      def mergeAll(xs: List[T],
                   xss: List[List[T]]): List[T] = xss match {
        case Nil => xs
        case h::t => mergeAll(merge(less)(xs, h), t)
      }

      val segs = segments
      mergeAll(Nil, segs)
    }

    def add(element: T): Sortable[T] = {

      def addSeg(seg: List[T],
                 segs: List[List[T]],
                 size: Int): List[List[T]] = {
        if(size % 2 == 0) {
          seg::segs
        } else {
          addSeg(merge(less)(seg, segs.head), segs.tail, size / 2)
        }
      }

      Sortable(less, size + 1, addSeg(List(element), segments, size))
    }
  }

  def apply[T](less: (T, T) => Boolean): Sortable[T] = {
    Sortable(less, 0, List.empty)
  }

  private def apply[T](less: (T, T) => Boolean,
               size: Int,
               segments: => List[List[T]]): Sortable[T] = {

    val ls = less
    val sz = size
    val segs = segments

    new SortableImpl[T] {

      def less: (T, T) => Boolean = ls

      def size: Int = sz

      def segments: List[List[T]] = segs

    }
  }

  def merge[T](less: (T, T) => Boolean)
                      (xs: List[T], ys: List[T]): List[T] = {

    def meg(pair: (List[T], List[T]), res: List[T]): List[T] = pair match {
      case (Nil, ys) => res.reverse ++ ys
      case (xs, Nil) => res.reverse ++ xs
      case (xs1@x::xs2, ys1@y::ys2) =>
        if (less(x, y)) {
          meg((xs2, ys1), x::res)
        } else {
          meg((xs1, ys2), y::res)
        }
    }

    meg((xs, ys), List.empty)
  }
}

