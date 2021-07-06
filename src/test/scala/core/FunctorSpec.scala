package core

import core.Functor.{given_Functor_List, given_Functor_Option}
import core.{Functor, FunctorLaws}

final class FunctorSpec extends munit.FunSuite with FunctorLaws:

  test("identity law") {
    assert(identity(List(1, 2, 3, 4, 5)))
    assert(identity(Option(1)))
  }

  test("compostion law") {
    assert(compostion(Option(1), i => i + 1, i => i * 2))
    assert(compostion(List(1, 2), i => i + 1, i => i * 2))
  }

  test("compose functors for list & option") {
    val flo = Functor.compose(summon[Functor[List]], summon[Functor[Option]])
    val result = flo.map(List(Option(1)))(i => i * 2)
    assert(result == List(Option(2)))
  }

