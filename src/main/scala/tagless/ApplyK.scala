package tagless

trait ApplyK[Alg[_[_]]] extends FunctorK[Alg]:
  def map2K[F[_], G[_], H[_]](left: Alg[F])(right: Alg[G])(f: [A] => (F[A], G[A]) => H[A]): Alg[H]
