package data

import core.{Functor, Monad}

final case class Cokleisli[F[_], A, B](run: F[A] => B):
  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(run andThen f)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(run(fa)).run(fa))

object Cokleisli:
  given [F[_], X]: Functor[Cokleisli[F, X, *]] with
    def map[A, B](fa: Cokleisli[F, X, A])(f: A => B): Cokleisli[F, X, B] = fa.map(f)

  given [F[_], X]: Monad[Cokleisli[F, X, *]] with
    def pure[A](a: A): Cokleisli[F, X, A] = Cokleisli[F, X, A](_ => a)
    def flatMap[A, B](fa: Cokleisli[F, X, A])(f: A => Cokleisli[F, X, B]): Cokleisli[F, X, B] =
      fa.flatMap(f)
