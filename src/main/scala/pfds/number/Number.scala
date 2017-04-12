package pfds.number

trait Nat[T <: Nat[_]] {

  def inc: T

  def dec: T

  def +(that: T): T

  def toInt: Int
}

object DecreaseZeroException extends RuntimeException("Can't decrease natural number zero") { }

