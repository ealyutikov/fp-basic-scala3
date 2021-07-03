package core

trait Semigroup[A]:
  def combine(x: A, y: A): A
  extension (x: A)
    infix def <+> (y: A): A = combine(x, y)
