package core

import core.Monoid.{given_Monoid_Boolean, given_Monoid_Int, given_Monoid_String}

final class MonoidSpec extends munit.FunSuite with MonoidLaws:

  test("associative law") {
    assert(associative(1, 2, 3))
    assert(associative(true, true, false))
    assert(associative("1", "2", "3"))
  }

  test("left identity law") {
    assert(leftIdentity(55))
    assert(leftIdentity("test"))
    assert(leftIdentity(true))
    assert(leftIdentity(false))
  }

  test("right identity law") {
    assert(rightIdentity(55))
    assert(rightIdentity("test"))
    assert(rightIdentity(true))
    assert(rightIdentity(false))
  }
