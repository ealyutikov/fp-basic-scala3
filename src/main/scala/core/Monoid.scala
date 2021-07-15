package core

trait Monoid[A] extends Semigroup[A]:
  def empty: A
  extension (x: A) infix def |+|(y: A): A = combine(x, y)

object Monoid:
  given Monoid[String] with
    def combine(x: String, y: String): String = x + y
    def empty: String = ""

  given Monoid[Int] with
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0

  given Monoid[Boolean] with
    def combine(x: Boolean, y: Boolean): Boolean = x && y
    def empty: Boolean = true

  given [A]: Monoid[List[A]] with
    def combine(x: List[A], y: List[A]): List[A] = x ::: y
    def empty: List[A] = Nil

  given [A, B](using A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)): (A, B) = (A.combine(x._1, y._1), B.combine(x._2, y._2))
    def empty: (A, B) = (A.empty, B.empty)

  given [A: Semigroup]: Monoid[Option[A]] with
    def empty: Option[A] = None
    def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match
        case (None, None)         => None
        case (Some(xx), None)     => Some(xx)
        case (None, Some(yy))     => Some(yy)
        case (Some(xx), Some(yy)) => Some(xx <+> yy)

  given [A]: Monoid[A => A] with
    def combine(f: A => A, g: A => A): A => A = f compose g
    def empty: A => A = a => a

  given [K, V](using M: Monoid[V]): Monoid[Map[K, V]] with
    def empty: Map[K, V] = Map.empty
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(empty) { (acc, k) =>
        acc.updated(k, M.combine(a.getOrElse(k, M.empty), b.getOrElse(k, M.empty)))
      }

trait MonoidLaws:
  def associative[A](a: A, b: A, c: A)(using M: Monoid[A]) =
    M.combine(M.combine(a, b), c) == M.combine(a, M.combine(b, c))

  def leftIdentity[A](a: A)(using M: Monoid[A]) =
    M.combine(M.empty, a) == a

  def rightIdentity[A](a: A)(using M: Monoid[A]) =
    M.combine(a, M.empty) == a
