package core

trait Monad[F[_]] extends Applicative[F]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(aTob => map(fa)(aTob))

  def flatten[A, B](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def flatTap[A, B](fa: F[A])(f: A => F[B]): F[A] =
    flatMap(fa)(a => as(f(a), a))

  extension [A, B] (fa: F[A])
    infix def >>= (f: A => F[B]): F[B] = flatMap(fa)(f)

object Monad:
  // WRONG, but compiles
  def compose[F[_], G[_]](F: Monad[F], G: Monad[G]): Monad[[X] =>> F[G[X]]] =
    new Monad[[X] =>> F[G[X]]]:
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        val nested = F.map(fga) { ga => G.map(ga) { a => f(a): F[G[B]] } : G[F[G[B]]] }: F[G[F[G[B]]]]
        flatten(nested)

  given Monad[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

  given Monad[Option] with
    def pure[A](a: A): Option[A] = Option(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  given [X]: Monad[Either[X, *]] with
    def pure[A](a: A): Either[X, A] = Right(a)
    def flatMap[A, B](fa: Either[X, A])(f: A => Either[X, B]): Either[X, B] = fa.flatMap(f)

trait MonadLaws:
  def associativity[F[_], A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(using F: Monad[F]) =
    F.flatMap(F.flatMap(fa)(f))(g) == F.flatMap(fa)(a => F.flatMap(f(a))(b => g(b)))

  def leftIdentity[F[_], A, B](a: A, f: A => F[B])(using F: Monad[F]) =
    F.flatMap(F.pure(a))(a => f(a)) == f(a)

  def rightIdentity[F[_], A](fa: F[A])(using F: Monad[F]) =
    F.flatMap(fa)(a => F.pure(a)) == fa
