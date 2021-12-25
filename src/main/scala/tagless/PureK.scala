package tagless

trait PureK[Alg[_[_]]] extends FunctorK[Alg]:
  def pureK[F[_]](gen: [A] => () => F[A]): Alg[F]
