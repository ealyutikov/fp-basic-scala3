package data

final case class Id[A](a: A):
  def map[B](f: A => B): Id[B] = Id(f(a))
  def flatMap[B](f: A => Id[B]): Id[B] = f(a)

  def extract: A = a
  def coflatten: Id[Id[A]] = Id(this)
  def coflatMap[B](f: Id[A] => B): Id[B] = coflatten.map(f)
