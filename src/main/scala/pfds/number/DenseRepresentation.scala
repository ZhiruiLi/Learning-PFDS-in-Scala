package pfds.number

private sealed trait Digit
private object Zero extends Digit
private object One extends Digit

private case class DenseRepresentation(num: List[Digit]) extends Nat[DenseRepresentation] {

  private def incDigs(digs: List[Digit]): List[Digit] = digs match {
    case Nil => List(One)
    case Zero::remain => One::remain
    case One::remain => Zero::incDigs(remain)
  }

  def inc: DenseRepresentation = DenseRepresentation(incDigs(num))

  def dec: DenseRepresentation = {
    def decDigs(digs: List[Digit]): List[Digit] = digs match {
      case Nil => throw DecreaseZeroException
      case One::remain => Zero::remain
      case Zero::remain => One::decDigs(remain)
    }
    DenseRepresentation(decDigs(num))
  }


  def +(that: DenseRepresentation): DenseRepresentation = {
    def add(a: List[Digit], b: List[Digit]): List[Digit] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (Zero::as, b1::bs) => b1::add(as, bs)
      case (a1::as, Zero::bs) => a1::add(as, bs)
      case (One::as, One::bs) => Zero::incDigs(add(as, bs))
    }
    DenseRepresentation(add(num, that.num))
  }

  def toInt: Int = {
    def convert(digs: List[Digit], curr: Int, acc: Int): Int = digs match {
      case Nil => acc
      case Zero::remain => convert(remain, curr * 2, acc)
      case One::remain => convert(remain, curr * 2, acc + curr)
    }
    convert(num, 1, 0)
  }
}


