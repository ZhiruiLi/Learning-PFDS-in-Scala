package pfds.queue

trait Queue[+T] {

  def isEmpty: Boolean

  // cons on the right
  def snoc[R >: T](x: R): Queue[R]

  def head: T

  def tail: Queue[T]
}

