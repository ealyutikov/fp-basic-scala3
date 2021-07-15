package core

trait Semigroup[A]:
  def combine(x: A, y: A): A
  extension (x: A) infix def <+>(y: A): A = combine(x, y)

trait SemigroupLaws:
  def associative[A](a: A, b: A, c: A)(using S: Semigroup[A]) =
    S.combine(S.combine(a, b), c) == S.combine(a, S.combine(b, c))
