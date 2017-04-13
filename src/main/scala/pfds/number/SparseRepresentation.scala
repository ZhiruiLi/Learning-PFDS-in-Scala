package pfds.number

sealed trait SparseRepresentation extends Nat[SparseRepresentation] {

  val weights: List[Int]

  private def carry(w: Int, weights: List[Int]): List[Int] = weights match {
    case Nil => List(w)
    case ww::remain =>
      if (w < ww) w::weights
      else if (w == ww) carry(w + ww, remain)
      else throw new RuntimeException(s"Illegal carry value: $w, where current weight is: $ww");
  }

  implicit private def genSparse(n: List[Int]): SparseRepresentation = new SparseRepresentation {
    val weights: List[Int] = n
  }

  override def inc: SparseRepresentation = carry(1, weights)

  override def dec: SparseRepresentation = {
    def borrow(w: Int, weights: List[Int]): List[Int] = weights match {
      case Nil => throw NegNatException
      case ww::remain =>
        if (w == ww) remain
        else if (w < ww) w::borrow(w * 2, weights)
        else throw new RuntimeException(s"Illegal borrow value: $w, where current weight is: $ww");
    }
    borrow(1, weights)
  }

  override def +(that: SparseRepresentation): SparseRepresentation = {
    def add(ws1: List[Int], ws2: List[Int]): List[Int] = (ws1, ws2) match {
      case (Nil, _) => ws2
      case (_, Nil) => ws1
      case (w1::remain1, w2::remain2) =>
        if (w1 < w2) w1::add(remain1, ws2)
        else if (w1 > w2) w2::add(ws1, remain2)
        else carry(w1 + w2, add(remain1, remain2))
    }
    add(weights, that.weights)
  }

  override def toInt: Int = weights.sum
}

object SparseRepresentation {

  def apply(num: Int): SparseRepresentation = {
    def gen(num: Int, acc: SparseRepresentation): SparseRepresentation = {
      if (num == 0) acc
      else gen(num - 1, acc.inc)
    }
    if (num < 0) throw NegNatException
    else gen(num, new SparseRepresentation { val weights = Nil })
  }
}