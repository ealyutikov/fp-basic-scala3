package tagless

trait FunctorK[Alg[f[_]]]:
  def mapK[F[_], G[_]](af: Alg[F])(fk: [A] => F[A] => G[A]): Alg[G]
