package pfds.number

sealed trait DigitBlock
case class ZerosBlock(count: Int) extends DigitBlock
case class OnesBlock(count: Int) extends DigitBlock

sealed trait SegmentedRepresentation extends Nat[SegmentedRepresentation] {

  val blocks: List[DigitBlock]

  private def zeros(i: Int, blks: List[DigitBlock]): List[DigitBlock] = blks match {
    case Nil => Nil
    case ZerosBlock(j)::remain => ZerosBlock(i + j)::remain
    case _ =>
      if (i == 0) blks
      else ZerosBlock(i)::blks
  }

  private def ones(i: Int, blks: List[DigitBlock]): List[DigitBlock] = blks match {
    case OnesBlock(j)::remain => OnesBlock(i + j)::remain
    case _ => if (i == 0) blks else OnesBlock(i)::blks
  }

  implicit private def genSeg(blks: List[DigitBlock]): SegmentedRepresentation = new SegmentedRepresentation {
    val blocks: List[DigitBlock] = blks
  }

  implicit private def unwrapBlocks(seg: SegmentedRepresentation): List[DigitBlock] = seg.blocks

  override def inc: SegmentedRepresentation = blocks match {
    case Nil => List(OnesBlock(1))
    case ZerosBlock(n)::remain => ones(1, zeros(n - 1, remain))
    case OnesBlock(n)::remain => ZerosBlock(n) :: remain.inc
  }

  def dec: SegmentedRepresentation = blocks match {
    case Nil => throw NegNatException
    case OnesBlock(n)::remain => zeros(1, ones(n - 1, remain))
    case ZerosBlock(n)::remain => OnesBlock(n) :: remain.dec
  }

  def +(that: SegmentedRepresentation): SegmentedRepresentation = {
    def add(a: List[DigitBlock], b: List[DigitBlock]): List[DigitBlock] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (OnesBlock(i)::remain1, ZerosBlock(j)::remain2) =>
        val m = math.min(i, j)
        ones(m, add(ones(i - m, remain1), zeros(j - m, remain2)))
      case (ZerosBlock(i)::remain1, OnesBlock(j)::remain2) =>
        val m = math.min(i, j)
        ones(m, add(zeros(i - m, remain1), ones(j - m, remain2)))
      case (ZerosBlock(i)::remain1, ZerosBlock(j)::remain2) =>
        val m = math.min(i, j)
        zeros(m, add(zeros(i - m, remain1), zeros(j - m, remain2)))
      case (OnesBlock(i)::remain1, OnesBlock(j)::remain2) =>
        val m = math.min(i, j)
        val tail = add(ones(i - m, remain1), ones(j - m, remain2))
        zeros(1, ones(m - 1, tail.inc))
    }
    add(blocks, that.blocks)
  }

  def toInt: Int = {
    def convert(blocks: List[DigitBlock], pos: Int, acc: Int): Int = blocks match {
      case Nil => acc
      case ZerosBlock(i)::remain => convert(remain, pos + i, acc)
      case OnesBlock(i)::remain => convert(ones(i - 1, remain), pos + 1, acc + (1 << pos))
    }
    convert(blocks, 0, 0)
  }
}

object SegmentedRepresentation {
  def apply(num: Int): SegmentedRepresentation = {
    def gen(num: Int, acc: SegmentedRepresentation): SegmentedRepresentation = {
      if (num == 0) acc
      else gen(num - 1, acc.inc)
    }
    if (num < 0) throw NegNatException
    else gen(num, new SegmentedRepresentation { val blocks: List[DigitBlock] = Nil })
  }
}
