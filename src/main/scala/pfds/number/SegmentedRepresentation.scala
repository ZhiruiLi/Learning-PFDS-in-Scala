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
    case _ =>
      if (i == 0) blks
      else OnesBlock(i)::blks
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

  def +(that: SegmentedRepresentation): SegmentedRepresentation = ???

  def toInt: Int = ???
}
