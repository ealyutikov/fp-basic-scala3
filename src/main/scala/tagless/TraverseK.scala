package tagless

import core.Applicative

trait TraverseK[Alg[_[_]]] extends FunctorK[Alg]:
  def traverseK[F[_], G[+_], H[_]](uf: Alg[F])(f: [A] => F[A] => G[H[A]])(using Applicative[G]): G[Alg[H]]

  def sequenceK[F[+_], G[_]](uf: Alg[[A] =>> F[G[A]]])(using Applicative[F]): F[Alg[G]] =
    traverseK(uf)([A] => (a: F[G[A]]) => a)
