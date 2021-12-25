package fixtures

final case class Person[F[_]](name: F[String], age: F[Int])
