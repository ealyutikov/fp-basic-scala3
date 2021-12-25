package tagless

trait ApplicativeK[Alg[_[_]]] extends ApplyK[Alg] with PureK[Alg]:
  override def mapK[F[_], G[_]](obj: Alg[F])(f: [A] => F[A] => G[A]): Alg[G] =
    map2K[F, [A] =>> Unit, G](obj: Alg[F])(pureK([A] => () => ()))([A] => (x: F[A], y: Unit) => f(x))
