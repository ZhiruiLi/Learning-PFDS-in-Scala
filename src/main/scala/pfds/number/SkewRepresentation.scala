package pfds.number

sealed trait SkewRepresentation extends Nat[SkewRepresentation] {

  val weights: List[Int]

  implicit private def genNum(w: List[Int]): SkewRepresentation =
    new SkewRepresentation { val weights: List[Int] = w }

  override def inc: SkewRepresentation = weights match {
    case w1::w2::remain if w1 == w2 => (1 + w1 + w2)::remain
    case _ => 1::weights
  }

  private def decWeights(weights: List[Int]): List[Int] = weights match {
    case Nil => throw NegNatException
    case 1::remain => remain
    case w::remain => (w / 2)::(w / 2)::remain
  }

  override def dec: SkewRepresentation = decWeights(weights)

  override def +(that: SkewRepresentation): SkewRepresentation = {

    def carry(w: Int, ws: List[Int]): List[Int] = ws match {
      case Nil => List(w)
      case w1::remain =>
        if (w1 < w) w1::carry(w, remain)
        else if (w1 == w) decWeights(carry(w + w1 + 1, remain))
        else w::ws
    }

    def add: (List[Int], List[Int]) => List[Int] = {
      case (Nil, ws) => ws
      case (ws, Nil) => ws
      case (ws1@(w1::ws11), ws2@(w2::ws22)) =>
        if (w1 < w2) w1::add(ws11, ws2)
        else if (w1 > w2) w2::add(ws1, ws22)
        else decWeights(carry(w1 + w2 + 1, add(ws11, ws22)))
    }

    add(weights, that.weights)
  }

  override def toInt: Int = weights.sum
}

object SkewRepresentation {

  def apply(num: Int): SkewRepresentation = {

    def convert(num: Int, pow2: Int, acc: List[Int]): List[Int] = {
      if (num == 0) {
        acc
      } else if (num == 1) {
        1::acc
      } else {
        if (pow2 <= num) convert(num, pow2 * 2, acc)
        else if (pow2 / 2 - 1 <= num) convert(num - (pow2 / 2 - 1), 1, (pow2 / 2 - 1)::acc)
        else throw new IllegalStateException(s"Illegal number pow2: $pow2")
      }
    }

    if (num < 0) throw NegNatException
    else new SkewRepresentation {
      override val weights: List[Int] = convert(num, 1, Nil)
    }
  }
}
