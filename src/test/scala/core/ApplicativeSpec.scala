package core

import core.Applicative.given_Applicative_Option

final class ApplicativeSpec extends munit.FunSuite with ApplicativeLaws:

  test("identity law") {
    assert(identity(List(1, 2, 3)))
    assert(identity(Option("test")))
  }

  test("product syntax") {
    assert(Some(1) *> Some(2) == Some(2))
    assert(Some(1) <* Some(2) == Some(1))
  }