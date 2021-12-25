package tagless

import fixtures.Person
import tagless.FunctorKSpec.given_FunctorK_Person
import tagless.FunctorKSpec.given_FunctorK_Person
import tagless.FunctionK.~>
import tagless.FunctionK.optionToList

final class FunctorKSpec extends munit.FunSuite:

  test("person option") {
    val p = Person(name = Option("test"), age = Option(10))
    val result = summon[FunctorK[Person]].mapK(p)(optionToList)
    assert(result == Person(name = List("test"), age = List(10)))

  }

object FunctorKSpec:
  given FunctorK[Person] with
    override def mapK[F[_], G[_]](personF: Person[F])(fk: [A] => F[A] => G[A]): Person[G] =
      Person[G](fk(personF.name), fk(personF.age))
