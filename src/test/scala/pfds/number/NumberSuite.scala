package pfds.number

import org.scalatest.FunSuite

trait NumberSuite[T <: Nat[T]] extends FunSuite {

  val Number: {
    def apply(num: Int): T
  }

  test("zero num") {
    assert(Number(0).toInt == 0)
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
  }

}

class NormalRepresentationSuite extends NumberSuite[NormalRepresentation] {
  val Number = NormalRepresentation
}

class DenseRepresentationSuite extends NumberSuite[DenseRepresentation] {
  val Number = DenseRepresentation
}
