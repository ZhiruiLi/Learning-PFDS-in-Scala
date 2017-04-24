package pfds.number

import normal._

package normal {
  case object Zero extends NormalRepresentation
  case class Succ(tl: NormalRepresentation) extends NormalRepresentation
}

sealed trait NormalRepresentation extends Nat[NormalRepresentation] {

  override def inc: NormalRepresentation = Succ(this)

  override def dec: NormalRepresentation = this match {
    case Zero => throw NegNatException
    case Succ(num) => num
  }

  override def +(that: NormalRepresentation): NormalRepresentation = {
    def add(a: NormalRepresentation, b: NormalRepresentation): NormalRepresentation = a match {
      case Zero => b
      case Succ(num) => add(num, Succ(b))
    }
    add(this, that)
  }

  override def toInt: Int = {
    def convert(num: NormalRepresentation, acc: Int): Int = num match {
      case Zero => acc
      case Succ(tl) => convert(tl, acc + 1)
    }
    convert(this, 0)
  }
}

object NormalRepresentation {

  def apply(num: Int): NormalRepresentation = {
    def gen(num: Int, acc: NormalRepresentation): NormalRepresentation = {
      if (num == 0) acc
      else gen(num - 1, acc.inc)
    }
    if (num < 0) throw NegNatException
    else gen(num, Zero)
  }
}