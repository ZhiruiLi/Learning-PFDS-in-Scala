package pfds.number

private sealed trait NormalRepresentation extends Nat[NormalRepresentation] {

  override def inc: NormalRepresentation = NormalSucc(this)

  override def dec: NormalRepresentation = this match {
    case NormalZero => throw DecreaseZeroException
    case NormalSucc(num) => num
  }

  override def +(that: NormalRepresentation): NormalRepresentation = {
    def add(a: NormalRepresentation, b: NormalRepresentation): NormalRepresentation = a match {
      case NormalZero => b
      case NormalSucc(num) => add(num, NormalSucc(b))
    }
    add(this, that)
  }

  override def toInt: Int = {
    def convert(num: NormalRepresentation, acc: Int): Int = num match {
      case NormalZero => acc
      case NormalSucc(tl) => convert(tl, acc + 1)
    }
    convert(this, 0)
  }
}

private case object NormalZero extends NormalRepresentation
private case class NormalSucc(tl: NormalRepresentation) extends NormalRepresentation

object NormalRepresentation {

  def apply(x: Int): Nat[_] = {
    def gen(x: Int, acc: NormalRepresentation): Nat[_] = if (x == 0) acc
    else gen(x - 1, acc.inc)
    gen(0, NormalZero)
  }
}