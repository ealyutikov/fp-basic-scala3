package core

import core.Monad.given_Monad_Option

final class MonadSpec extends munit.FunSuite with MonadLaws {

  test("flatMap syntax") {
    val result = Option(1) >>= { int => Option(int.toString) }
    assert(result == Option("1"))
  }

  test("left identity law") {
    val result = Option(1) >>= { int => Option(int.toString) }
    assert(leftIdentity(10, int => Option(int.toString)))
  }

  test("right identitty law") {
    assert(rightIdentity(List(1, 2, 3)))
    assert(rightIdentity(Option("hello")))
  }

}
