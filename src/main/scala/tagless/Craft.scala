package tagless

import core.Applicative
import tagless.RepresentableK.Rep
import data.Id

trait Craft[Alg[_[_]]] extends RepresentableK[Alg] with TraverseK[Alg]:
  def craft[F[+_]: Applicative, G[_]](gain: [A] => Rep[Alg, A] => F[G[A]]): F[Alg[G]]
  
  override def traverseK[F[_], G[+_], H[_]](uf: Alg[F])(f: [A] => F[A] => G[H[A]])(using Applicative[G]): G[Alg[H]] =
    craft[G, H]([A] => (frep: Rep[Alg, A]) => f(frep(uf)))
