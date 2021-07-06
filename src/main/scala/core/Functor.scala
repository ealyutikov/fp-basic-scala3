package core

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)
  def void[A](fa: F[A]): F[Unit] = as(fa, ())
  def unzip[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

object Functor:
  def compose[F[_], G[_]](F: Functor[F], G: Functor[G]): Functor[[X] =>> F[G[X]]] =
    new Functor[[X] =>> F[G[X]]]:
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(ga => G.map(ga)(a => f(a)))

  given Functor[List] with
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  given Functor[Option] with
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

  given [X]: Functor[Either[X, *]] with
    def map[A, B](fa: Either[X, A])(f: A => B): Either[X, B] = fa.map(f)

  given [X]: Functor[X => *] with
    def map[A, B](f: X => A)(g: A => B): X => B = f andThen g

trait FunctorLaws:
  def identity[F[_], A](fa: F[A])(using F: Functor[F]): Boolean =
    F.map(fa)(a => a) == fa

  def compostion[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(using F: Functor[F]) : Boolean =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)