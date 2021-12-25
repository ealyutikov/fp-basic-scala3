package tagless

import RepresentableK.*

trait RepresentableK[Alg[_[_]]] extends ApplicativeK[Alg]:
  def tabulate[F[_]](gain: [A] => Rep[Alg, A] => F[A]): Alg[F]

  override def pureK[F[_]](gen: [A] => () => F[A]): Alg[F] =
    tabulate([A] => (rep: Rep[Alg, A]) => gen[A]())

  override def map2K[F[_], G[_], H[_]](left: Alg[F])(right: Alg[G])(f: [A] => (F[A], G[A]) => H[A]): Alg[H] =
    tabulate([A] => (rep: Rep[Alg, A]) => f[A](rep(left), rep(right)))

object RepresentableK:
  type Rep[-Alg[f[_]], A] = [F[_]] => Alg[F] => F[A]
