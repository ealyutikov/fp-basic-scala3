package core

trait Foldable[F[_]]:
  def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(using M: Monoid[B]): B =
    foldLeft(fa, M.empty)((b, a) => M.combine(b, f(a)))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(identity)

  def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[Unit] =
    foldLeft(fa, G.pure(()))((acc, a) => G.map2(acc, f(a))((_, _) => ()))

  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

object Foldable:
  def compose[F[_], G[_]](F: Foldable[F], G: Foldable[G]): Foldable[[X] =>> F[G[X]]] =
    new Foldable[[X] =>> F[G[X]]]:
      def foldLeft[A, B](fa: F[G[A]], initial: B)(f: (B, A) => B): B =
        F.foldLeft(fa, initial)((acc, ga) => G.foldLeft(ga, acc)(f))
      def foldRight[A, B](fa: F[G[A]], initial: B)(f: (A, B) => B): B =
        F.foldRight(fa, initial)((ga, acc) => G.foldRight(ga, acc)(f))

  given Foldable[List] with
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B): B = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B): B = fa.foldRight(initial)(f)

  given Foldable[Option] with
    def foldLeft[A, B](fa: Option[A], initial: B)(f: (B, A) => B): B = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: Option[A], initial: B)(f: (A, B) => B): B = fa.foldRight(initial)(f)
