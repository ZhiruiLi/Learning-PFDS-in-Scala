package pfds.list

trait RandomAccessList[+T] {

  def isEmpty: Boolean

  def ::[R >: T](elem: R): RandomAccessList[R]

  def head: T

  def tail: RandomAccessList[T]

  def apply(idx: Int): T

  def updated[R >: T](idx: Int, elem: R): RandomAccessList[R]

}

private[list] sealed trait Tree[+T] {
  def count: Int
}
private[list] case class Leaf[+T](elem: T) extends Tree[T] {
  val count = 1
}
private[list] case class Branch[+T](count: Int, left: Tree[T], right: Tree[T]) extends Tree[T]


private[list] case class RandomAccessListImpl[+T](trees: List[Option[Tree[T]]]) extends RandomAccessList[T] {

  def isEmpty: Boolean = trees.isEmpty

  private[this] def linkTree[R](t1: Tree[R], t2: Tree[R]): Tree[R] = {
    Branch(t1.count + t2.count, t1, t2)
  }

  private[this] def insTree[R](tree: Tree[R], trees: List[Option[Tree[R]]]):
  List[Option[Tree[R]]] = trees match {
    case Nil => List(Some(tree))
    case None::remain => Some(tree)::remain
    case Some(tree1)::remain => None::insTree(linkTree(tree, tree1), remain)
  }

  def ::[R >: T](elem: R): RandomAccessList[R] =
    RandomAccessListImpl(insTree(Leaf(elem), trees))

  private[this] def borrowTree(trees: List[Option[Tree[T]]]):
  (Tree[T], List[Option[Tree[T]]]) = trees match {
    case Nil => throw new IndexOutOfBoundsException
    case List(Some(tree)) => (tree, Nil)
    case Some(tree)::remain => (tree, None::remain)
    case None::remain =>
      val (Branch(_, l, r), remain1) = borrowTree(remain)
      (l, Some(r)::remain1)
  }

  def head: T = {
    val (Leaf(x), _) = borrowTree(trees)
    x
  }

  def tail: RandomAccessList[T] =
    RandomAccessListImpl(borrowTree(trees)._2)

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
    def helper(trees: List[Option[Tree[T]]], idx: Int): T = trees match {
      case Nil => throw new IndexOutOfBoundsException
      case None::remain => helper(remain, idx)
      case Some(tree)::remain =>
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
    def helper(trees: List[Option[Tree[R]]], idx: Int):
    List[Option[Tree[R]]] = trees match {
      case Nil => throw new IndexOutOfBoundsException
      case None::remain => None::helper(remain, idx)
      case (curr@Some(tree))::remain =>
        if (tree.count > idx) {
          Some(updateTree(tree, idx, elem))::remain
        } else {
          curr::helper(remain, idx - tree.count)
        }
    }
    if (idx < 0) {
      throw new IndexOutOfBoundsException
    } else {
      RandomAccessListImpl(helper(trees, idx))
    }
  }

  def toPrettyString: String = {
    def convertTree(tree: Tree[T]): Stream[T] = tree match {
      case Leaf(x) => Stream(x)
      case Branch(_, l, r) => convertTree(l) ++ convertTree(r)
    }
    val ss = trees.flatMap {
      case None => Stream.empty
      case Some(tree) => convertTree(tree)
    }
    "[" ++ ss.mkString(", ") ++ "]"
  }
}

object RandomAccessList {
  val empty = RandomAccessListImpl(Nil)
}
