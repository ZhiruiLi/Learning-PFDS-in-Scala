package pfds.number

import dense._

package dense {
  sealed trait Digit
  case object Zero extends Digit
  case object One extends Digit
}

sealed trait DenseRepresentation extends Nat[DenseRepresentation] {

  val num: List[Digit]

  implicit private def genDense(newNum: List[Digit]):
  DenseRepresentation = new DenseRepresentation {
    val num: List[Digit] = newNum
  }

  private def incDigs(digs: List[Digit]): List[Digit] = digs match {
    case Nil => List(One)
    case Zero::remain => One::remain
    case One::remain => Zero::incDigs(remain)
  }

  override def inc: DenseRepresentation = incDigs(num)

  override def dec: DenseRepresentation = {
    def decDigs(digs: List[Digit]): List[Digit] = digs match {
      case Nil => throw NegNatException
      case One::remain => Zero::remain
      case Zero::remain => One::decDigs(remain)
    }
    decDigs(num)
  }

  override def +(that: DenseRepresentation): DenseRepresentation = {
    def add(a: List[Digit], b: List[Digit]): List[Digit] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (Zero::as, b1::bs) => b1::add(as, bs)
      case (a1::as, Zero::bs) => a1::add(as, bs)
      case (One::as, One::bs) => Zero::incDigs(add(as, bs))
    }
    add(num, that.num)
  }

  override def toInt: Int = {
    def convert(digs: List[Digit], curr: Int, acc: Int): Int = digs match {
      case Nil => acc
      case Zero::remain => convert(remain, curr * 2, acc)
      case One::remain => convert(remain, curr * 2, acc + curr)
    }
    convert(num, 1, 0)
  }
}

object DenseRepresentation {

  def apply(num: Int): DenseRepresentation = {
    def gen(num: Int, acc: DenseRepresentation): DenseRepresentation = {
      if (num == 0) acc
      else gen(num - 1, acc.inc)
    }
    if (num < 0) throw NegNatException
    else gen(num, new DenseRepresentation { val num = Nil })
  }
}
