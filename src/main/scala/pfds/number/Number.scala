package pfds.number

trait Nat[T <: Nat[T]] {

  def inc: Nat[T]

  def dec: Nat[T]

  def +(that: T): Nat[T]

  def toInt: Int
}

object NegNatException extends RuntimeException("Natural number can't be negative") { }

