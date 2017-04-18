package pfds.list

trait RandomAccessList[+T] {

  def isEmpty: Boolean

  def ::[R >: T](elem: R): RandomAccessList[R]

  def head: T

  def tail: RandomAccessList[T]

  def apply(idx: Int): T

  def updated[R >: T](idx: Int, elem: R): RandomAccessList[R]
}

