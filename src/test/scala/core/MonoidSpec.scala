package core

import core.Monoid.{given_Monoid_Int, given_Monoid_String, given_Monoid_Boolean}
import core.Monoid.Extensions.*

final class MonoidSpec extends munit.FunSuite:

  private val intM = summon[Monoid[Int]]
  private val strM = summon[Monoid[String]]
  private val boolM = summon[Monoid[Boolean]]

  test("associative law") {
    assert((("1" |+| "2") |+| "3") == ("1" |+| ("2" |+| "3")))
    assert(((1 |+| 2) |+| 3) == (1 |+| (2 |+| 3)))
    assert(((true |+| false) |+| true) == (true |+| (false |+| true)))
  }

  test("left identity law") {
    assert((intM.empty |+| 55) == 55)
    assert((strM.empty |+| "hello") == "hello")
    assert((boolM.empty |+| true) == true)
  }

  test("right identity law") {
    assert((55 |+| intM.empty) == 55)
    assert(("hello" |+| strM.empty) == "hello")
    assert((false |+| boolM.empty) == false)
  }

  test("collect list with monoid") {
    assert(List(1,2,3,4,5).collectM == 15)
  }

