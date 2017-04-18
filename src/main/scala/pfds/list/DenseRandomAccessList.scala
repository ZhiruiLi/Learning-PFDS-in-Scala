package pfds.list

sealed trait Digit[+T]
case object Zero extends Digit[Nothing]
case class One[+T](tree: Tree[T]) extends Digit[T]

sealed trait Tree[+T] {
  def count: Int
}
case class Leaf[+T](elem: T) extends Tree[T] {
  val count = 1
}
case class Branch[+T](count: Int, left: Tree[T], right: Tree[T]) extends Tree[T]

case class DenseRandomAccessList[+T](trees: List[Digit[T]]) extends RandomAccessList[T] {

  def isEmpty: Boolean = trees.isEmpty

  private[this] def linkTree[R](t1: Tree[R], t2: Tree[R]): Tree[R] = {
    Branch(t1.count + t2.count, t1, t2)
  }

  private[this] def insTree[R](tree: Tree[R], trees: List[Digit[R]]):
  List[Digit[R]] = trees match {
    case Nil => List(One(tree))
    case Zero::remain => One(tree)::remain
    case One(tree1)::remain => Zero::insTree(linkTree(tree, tree1), remain)
  }

  def ::[R >: T](elem: R): RandomAccessList[R] =
    DenseRandomAccessList(insTree(Leaf(elem), trees))

  private[this] def borrowTree(trees: List[Digit[T]]):
  (Tree[T], List[Digit[T]]) = trees match {
    case Nil => throw new IndexOutOfBoundsException
    case List(One(tree)) => (tree, Nil)
    case One(tree)::remain => (tree, Zero::remain)
    case Zero::remain =>
      val (Branch(_, l, r), remain1) = borrowTree(remain)
      (l, One(r)::remain1)
  }

  def head: T = {
    val (Leaf(x), _) = borrowTree(trees)
    x
  }

  def tail: RandomAccessList[T] =
    DenseRandomAccessList(borrowTree(trees)._2)

  private[this] def lookupTree(tree: Tree[T], idx: Int): T = tree match {
    case Leaf(x) if idx == 0 => x
    case Leaf(_) => throw new IndexOutOfBoundsException
    case Branch(count, l, r) =>
      if (idx < count / 2) {
        lookupTree(l, idx)
      } else {
        lookupTree(r, idx - count / 2)
      }
  }

  def apply(idx: Int): T = {
    def helper(trees: List[Digit[T]], idx: Int): T = trees match {
      case Nil => throw new IndexOutOfBoundsException
      case Zero::remain => helper(remain, idx)
      case One(tree)::remain =>
        if (tree.count > idx) {
          lookupTree(tree, idx)
        } else {
          helper(remain, idx - tree.count)
        }
    }
    if (idx < 0) {
      throw new IndexOutOfBoundsException
    } else {
      helper(trees, idx)
    }
  }

  private[this] def updateTree[R](tree: Tree[R], idx: Int, elem: R): Tree[R] = tree match {
    case Leaf(_) if idx == 0 => Leaf(elem)
    case Leaf(_) => throw new IndexOutOfBoundsException
    case Branch(count, l, r) =>
      if (idx < count / 2) {
        Branch(count, updateTree(l, idx, elem), r)
      } else {
        Branch(count, l, updateTree(r, idx - count / 2, elem))
      }
  }

  def updated[R >: T](idx: Int, elem: R): RandomAccessList[R] = {
    def helper(trees: List[Digit[R]], idx: Int):
    List[Digit[R]] = trees match {
      case Nil => throw new IndexOutOfBoundsException
      case Zero::remain => Zero::helper(remain, idx)
      case (curr@One(tree))::remain =>
        if (tree.count > idx) {
          One(updateTree(tree, idx, elem))::remain
        } else {
          curr::helper(remain, idx - tree.count)
        }
    }
    if (idx < 0) {
      throw new IndexOutOfBoundsException
    } else {
      DenseRandomAccessList(helper(trees, idx))
    }
  }

  def toPrettyString: String = {
    def convertTree(tree: Tree[T]): Stream[T] = tree match {
      case Leaf(x) => Stream(x)
      case Branch(_, l, r) => convertTree(l) ++ convertTree(r)
    }
    val ss = trees.flatMap {
      case Zero => Stream.empty
      case One(tree) => convertTree(tree)
    }
    "[" ++ ss.mkString(", ") ++ "]"
  }
}

object DenseRandomAccessList {
  val empty = DenseRandomAccessList(Nil)
}
