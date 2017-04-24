package pfds.list

import pfds.list.skew._

package skew {
  sealed trait Tree[+T]
  case class Leaf[+T](elem: T) extends Tree[T]
  case class Branch[+T](elem: T, left: Tree[T], right: Tree[T]) extends Tree[T]
}

case class SkewRandomAccessList[+T](trees: List[(Int, Tree[T])]) extends RandomAccessList[T] {

  implicit private def genList[T](trees: List[(Int, Tree[T])]):
  SkewRandomAccessList[T] = SkewRandomAccessList(trees)

  override def isEmpty: Boolean = trees.isEmpty

  override def ::[R >: T](elem: R): SkewRandomAccessList[R] = trees match {
    case (n1, t1)::(n2, t2)::remain if n1 == n2 =>
      (n1 + n2 + 1, Branch(elem, t1, t2))::remain
    case _ => (1, Leaf(elem))::trees
  }

  private def illegalHeadState(hd: (Int, Tree[_])) =
    new IllegalStateException(s"Illegal head of Skew Random Access List: $hd")

  override def head: T = trees match {
    case Nil => throw new IndexOutOfBoundsException
    case (1, Leaf(elem))::_ => elem
    case (_, Branch(elem, _, _))::_ => elem
    case _ => throw illegalHeadState(trees.head)
  }

  override def tail: SkewRandomAccessList[T] = trees match {
    case Nil => throw new IndexOutOfBoundsException
    case (1, Leaf(_))::remain => remain
    case (n, Branch(_, t1, t2))::remain => (n / 2, t1)::(n / 2, t2)::remain
    case _ => throw illegalHeadState(trees.head)
  }

  override def apply(idx: Int): T = {

    def applyList(idx: Int, lst: List[(Int, Tree[T])]): T = lst match {
      case Nil => throw new IndexOutOfBoundsException
      case (n, tree)::remain =>
        if (idx < n) applyTree(idx, n, tree)
        else applyList(idx - n, remain)
    }

    def applyTree(idx: Int, count: Int, tree: Tree[T]): T = tree match {
      case Leaf(elem) => if (idx == 0) elem else throw new IndexOutOfBoundsException
      case Branch(elem, t1, t2) =>
        if (idx == 0) {
          elem
        } else {
          if (idx <= count / 2) applyTree(idx - 1, count / 2, t1)
          else applyTree(idx - count / 2 - 1, count / 2, t2)
        }
    }

    if (idx < 0) throw new IndexOutOfBoundsException
    else applyList(idx, trees)
  }

  override def updated[R >: T](idx: Int, elem: R): SkewRandomAccessList[R] = {

    def updatedList(idx: Int, lst: List[(Int, Tree[R])]): List[(Int, Tree[R])] = lst match {
      case Nil => throw new IndexOutOfBoundsException
      case (curr@(n, tree))::remain =>
        if (idx < n) (n, updatedTree(idx, n, tree))::remain
        else curr::updatedList(idx - n, remain)
    }

    def updatedTree(idx: Int, count: Int, tree: Tree[R]): Tree[R] = tree match {
      case Leaf(_) => if (idx == 0) Leaf(elem) else throw new IndexOutOfBoundsException
      case Branch(oldE, t1, t2) =>
        if (idx == 0) {
          Branch(elem, t1, t2)
        } else {
          if (idx <= count / 2) Branch(oldE, updatedTree(idx - 1, count / 2, t1), t2)
          else Branch(oldE, t1, updatedTree(idx - count / 2 - 1, count / 2, t2))
        }
    }

    if (idx < 0) throw new IndexOutOfBoundsException
    else updatedList(idx, trees)
  }
}

object SkewRandomAccessList {

  val empty: RandomAccessList[Nothing] = SkewRandomAccessList(Nil)

  def apply[T](elems: List[T]): RandomAccessList[T] = elems match {
    case Nil => empty
    case e::remain => e::apply(remain)
  }

  def apply[T](elems: T*): RandomAccessList[T] = apply(elems.toList)
}
