//package tagless
//
//import fixtures.Person
//import tagless.FunctorKSpec.optionToList
//
//final class FunctorKSpec extends munit.FunSuite:
//
//  test("person option") {
//    val p = Person(name = Option("test"), age = Option(10))
//
//    val s = summon[FunctorK[Person]]
//
//    println(121212)
//  }
//
//object FunctorKSpec:
//  val optionToList: Option ~> List = [A] => (opt: Option[A]) => opt.toList
//  val listToOption: List ~> Option = [A] => (list: List[A]) => list.headOption
//
//  given FunctorK[Person] with
//    override def mapK[F[_], G[_]](personF: Person[F])(fk: [A] => F[A] => G[A]): Person[G] =
//      Person[G](fk(personF.name), fk(personF.age))
