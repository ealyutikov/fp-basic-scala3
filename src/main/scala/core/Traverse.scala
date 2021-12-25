package core

import data.Id

trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fa: F[G[A]]): G[F[A]] =
    traverse(fa)(identity)

object Traverse:
  def compose[F[_], G[_]](TF: Traverse[F], TG: Traverse[G])(FF: Functor[F], GF: Functor[G])(
    F: Foldable[F],
    G: Foldable[G]
  ): Traverse[[X] =>> F[G[X]]] =
    new Traverse[[X] =>> F[G[X]]]:
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        TF.traverse(fga)(ga => TG.traverse(ga)(f))

      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        FF.map(fga)(ga => GF.map(ga)(f))

      def foldLeft[A, B](fga: F[G[A]], b: B)(f: (B, A) => B): B =
        F.foldLeft(fga, b)((b, ga) => G.foldLeft(ga, b)(f))

      def foldRight[A, B](fga: F[G[A]], b: B)(f: (A, B) => B): B =
        F.foldRight(fga, b)((ga, lb) => G.foldRight(ga, lb)(f))
