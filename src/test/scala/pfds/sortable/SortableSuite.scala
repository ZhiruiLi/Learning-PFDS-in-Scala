package pfds.sortable

import org.scalatest.FunSuite

class SortableSuite extends FunSuite {

  test("A empty Sortable should produce a empty String") {
    val lst: List[Nothing] = Sortable[Nothing]((_, _) => true).sort
    assert(lst.isEmpty)
  }

  test("A Sortable contain one element should produce a List with only one element") {
    val lst = Sortable[Int]((_, _) => true).add(1).sort
    assert(lst.length == 1)
    assert(lst.head == 1)
  }

  test("merge function in Sortable should merge two ordered List in order") {
    val lst1 = Sortable.merge[Int] {
      case (x, y) => x < y
    }(List(1, 3, 4), List(2, 5, 6, 7, 10, 20, 100))
    val lst2 = lst1.zip(lst1.tail)
    assert(lst2.forall {
      case (x, y) => x < y
    })
  }

  test("A Sortable contain n Integers should produce a List with sorted Integers") {
    val lst1 = Sortable[Int]((x, y) => x < y).add(3).add(1).add(2).add(5).add(4).sort
    val lst2 = lst1.zip(lst1.tail)
    assert(lst2.forall { case (x, y) => x < y })
  }
}
