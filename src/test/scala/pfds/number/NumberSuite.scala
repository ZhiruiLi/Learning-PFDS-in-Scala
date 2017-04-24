package pfds.number

import org.scalatest.FunSuite

trait NumberSuite[T <: Nat[T]] extends FunSuite {

  val Number: {
    def apply(num: Int): T
  }

  test("to int") {
    assert(Number(0).toInt == 0)
    assert(Number(1).toInt == 1)
    assert(Number(10).toInt == 10)
    assert(Number(453).toInt == 453)
    assertThrows[NegNatException.type](Number(-1))
  }

  test("increase number") {
    val zero = Number(0)
    assert(zero.toInt == 0)
    assert(zero.inc.toInt == 1)
    assert(zero.inc.inc.toInt == 2)
    assert(zero.toInt == 0)
  }

  test("decrease number") {
    val two = Number(2)
    assert(two.toInt == 2)
    assert(two.dec.toInt == 1)
    assert(two.dec.dec.toInt == 0)
    assertThrows[NegNatException.type](two.dec.dec.dec)
    assert(two.toInt == 2)
  }

  test("plus numbers") {
    val one = Number(1)
    val two = Number(2)
    assert((one + two).toInt == 3)
    assert((two + one).toInt == 3)
    assert((one.inc.dec + Number(0)).toInt == 1)
    assert((Number(12) + Number(21)).toInt == (12 + 21))
  }
}

class NormalRepresentationSuite extends NumberSuite[NormalRepresentation] {
  val Number = NormalRepresentation
}

class DenseRepresentationSuite extends NumberSuite[DenseRepresentation] {
  val Number = DenseRepresentation
}

class SparseRepresentationSuite extends NumberSuite[SparseRepresentation] {
  val Number = SparseRepresentation
}

class SegmentedRepresentationSuite extends NumberSuite[SegmentedRepresentation] {
  val Number = SegmentedRepresentation
}

class SkewRepresentationSuite extends NumberSuite[SkewRepresentation] {
  val Number = SkewRepresentation
}
