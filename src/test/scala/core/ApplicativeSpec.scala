package core

final class ApplicativeSpec extends munit.FunSuite with ApplicativeLaws:

  test("identity law") {
    assert(identity(List(1, 2, 3)))
    assert(identity(Option("test")))
  }