package pfds.queue

trait Queue[+T] {

  def isEmpty: Boolean

  // cons on the right
  def snoc[R >: T](x: R): Queue[R]

  final def #::[R >: T](x: R): Queue[R] = snoc(x)

  def head: T

  def tail: Queue[T]
}

