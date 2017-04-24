package pfds.list

import segmented._

package segmented {
  sealed trait DigitBlock[+T]
  case class Zeros(num: Int) extends DigitBlock[Nothing]
  case class Ones[+T](trees: List[BinomialTree[T]]) extends DigitBlock[T]

  case class BinomialTree[+T](elem: T, children: List[BinomialTree[T]] = Nil) {
    lazy val count: Int = children.map(_.count).sum + 1
    lazy val rank: Int = if (children.isEmpty) 1 else children.head.rank + 1
    def merge[R >: T](that: BinomialTree[R]): BinomialTree[R] = {
      assert(rank == that.rank)
      copy(children = that::children)
    }
  }
}

case class SegmentedRandomAccessList[+T](blocks: List[DigitBlock[T]])
  extends RandomAccessList[T] {

  def isEmpty: Boolean = blocks.isEmpty

  implicit private def genList[R](blks: List[DigitBlock[R]]):
  SegmentedRandomAccessList[R] = SegmentedRandomAccessList(blks)

  private def zeros[R](i: Int, blks: List[DigitBlock[R]]): List[DigitBlock[R]] = blks match {
    case Nil => Nil
    case Zeros(j)::remain => Zeros(i + j)::remain
    case _ => if (i == 0) blks else Zeros(i)::blks
  }

  private def ones[A, B >: A](trees: List[BinomialTree[B]], blks: List[DigitBlock[A]]):
  List[DigitBlock[B]] = blks match {
    case Ones(trees1)::remain => Ones(trees ++ trees1)::remain
    case _ => if (trees.isEmpty) blks else Ones(trees)::blks
  }

  private def insTree[R](tree: BinomialTree[R], blks: List[DigitBlock[R]]):
  List[DigitBlock[R]] = blks match {
    case Nil => List(Ones(List(tree)))
    case Zeros(i)::remain => ones(List(tree), zeros(i - 1, remain))
    case Ones(trees)::remain =>
      val newTree = trees.foldLeft(tree)(_.merge(_))
      zeros(trees.length, insTree(newTree, remain))
  }

  def ::[R >: T](elem: R): RandomAccessList[R] = insTree(BinomialTree(elem), blocks)

  private def borrowTree[R](blks: List[DigitBlock[R]]):
  (BinomialTree[R], List[DigitBlock[R]]) = blks match {
    case Nil => throw new IndexOutOfBoundsException
    case Zeros(_)::remain => borrowTree(remain)
    case Ones(trees)::remain =>
      (trees.head, ones(trees.head.children.reverse, zeros(1, ones(trees.tail, remain))))
  }

  def head: T = borrowTree(blocks)._1.elem

  def tail: RandomAccessList[T] = borrowTree(blocks)._2

  private def countAll(trees: List[BinomialTree[_]]): Int =
    trees.foldLeft(0)(_ + _.count)

  def applyTreeList[R](trees: List[BinomialTree[R]], idx: Int): R = {

    def applyTree(tree: BinomialTree[R], idx: Int): R = {
      if (idx == 0) tree.elem
      else applyTreeList(tree.children.reverse, idx - 1)
    }

    trees match {
      case Nil => throw new IndexOutOfBoundsException
      case tree::remain =>
        val count = tree.count
        if (idx < count) applyTree(tree, idx)
        else applyTreeList(remain, idx - count)
    }
  }

  def apply(idx: Int): T = {

    def applyBlocks(blks: List[DigitBlock[T]], idx: Int): T = blks match {
      case Nil => throw new IndexOutOfBoundsException
      case Zeros(_)::remain => applyBlocks(remain, idx)
      case Ones(trees)::remain =>
        val count = countAll(trees)
        if (idx < count) applyTreeList(trees, idx)
        else applyBlocks(remain, idx - count)
    }

    if (idx < 0) throw new IndexOutOfBoundsException
    else applyBlocks(blocks, idx)
  }

  def updatedTreeList[R](trees: List[BinomialTree[R]], idx: Int, elem: R): List[BinomialTree[R]] = {

    def updatedTree(tree: BinomialTree[R], idx: Int, elem: R): BinomialTree[R] = {
      if (idx == 0) BinomialTree(elem, tree.children)
      else BinomialTree(tree.elem, updatedTreeList(tree.children.reverse, idx - 1, elem).reverse)
    }

    trees match {
      case Nil => throw new IndexOutOfBoundsException
      case tree::remain =>
        val count = tree.count
        if (idx < count) updatedTree(tree, idx, elem)::remain
        else tree::updatedTreeList(remain, idx - count, elem)
    }
  }

  def updated[R >: T](idx: Int, elem: R): RandomAccessList[R] = {

    def updatedBlocks(blks: List[DigitBlock[T]], idx: Int, elem: R): List[DigitBlock[R]] = blks match {
      case Nil => throw new IndexOutOfBoundsException
      case (z@Zeros(_))::remain => z::updatedBlocks(remain, idx, elem)
      case (o@Ones(trees))::remain =>
        val count = countAll(trees)
        if (idx < count) Ones(updatedTreeList(trees, idx, elem))::remain
        else o::updatedBlocks(remain, idx - count, elem)
    }

    if (idx < 0) throw new IndexOutOfBoundsException
    else updatedBlocks(blocks, idx, elem)
  }

}

object SegmentedRandomAccessList {
  val empty: SegmentedRandomAccessList[Nothing] =
    SegmentedRandomAccessList[Nothing](Nil)
}
