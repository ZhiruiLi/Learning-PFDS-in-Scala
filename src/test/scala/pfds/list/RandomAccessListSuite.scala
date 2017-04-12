package pfds.list

import org.scalatest.FunSuite

class RandomAccessListSuite extends FunSuite {

  test("empty list") {
    assert(RandomAccessList.empty == RandomAccessList.empty)
    assert(RandomAccessList.empty.isEmpty)
  }

  test(":: operator append an element in the front of a list") {
    val lst1 = RandomAccessList.empty
    val lst2 = 1::lst1
    assert(!lst2.isEmpty)
    assert(lst2.head == 1)
    assert(lst2.tail == lst1)
    val lst3 = 2::1::lst1
    assert(!lst3.isEmpty)
    assert(lst3.head == 2)
    assert(lst3.tail == lst2)
    assert(lst1 == RandomAccessList.empty)
  }

  test("head method fetch the first element of a list") {
    val lst1 = 1::RandomAccessList.empty
    assert(lst1.head == 1)
    assert((3::2::lst1).head == 3)
    assertThrows[IndexOutOfBoundsException](RandomAccessList.empty.head)
  }

  test("tail method fetch the tailing list of a list") {
    val lst1 = 1::RandomAccessList.empty
    assert(lst1.tail == RandomAccessList.empty)
    assert((3::2::lst1).tail.tail.tail == RandomAccessList.empty)
    assertThrows[IndexOutOfBoundsException](RandomAccessList.empty.tail)
  }

  test("apply method fetch one element in the list associated with the given index") {
    val lst1 = RandomAccessList.empty
    assertThrows[IndexOutOfBoundsException](lst1(0))
    val lst2 = 1::2::lst1
    assertThrows[IndexOutOfBoundsException](lst2(-1))
    assertThrows[IndexOutOfBoundsException](lst2(2))
    assert(lst2(0) == 1)
    assert(lst2(1) == 2)
    val lst3 = 0::1::2::3::4::5::6::lst1
    assert(lst3(6) == 6)
    assert(lst3(3) == 3)
    assert(lst3(0) == 0)
  }

  test("updated method update one element in the list associated with the given index") {
    val lst1 = RandomAccessList.empty
    assertThrows[IndexOutOfBoundsException](lst1.updated(0, 0))
    val lst2 = 1::2::lst1
    assertThrows[IndexOutOfBoundsException](lst2.updated(-1, 0))
    assertThrows[IndexOutOfBoundsException](lst2.updated(2, 0))
    assert(lst2.updated(0, 0) == (0::2::lst1))
    assert(lst2.updated(1, 0) == (1::0::lst1))
    val lst3 = 0::1::2::3::4::5::6::lst1
    assert(lst3.updated(6, 0) == 0::1::2::3::4::5::0::lst1)
    assert(lst3.updated(3, 0) == 0::1::2::0::4::5::6::lst1)
    assert(lst3.updated(0, 7) == 7::1::2::3::4::5::6::lst1)
    assert(lst3.updated(1, 8).updated(5, 9) == lst3.updated(5, 9).updated(1, 8))
  }

}
