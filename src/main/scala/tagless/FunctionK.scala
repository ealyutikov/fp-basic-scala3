package tagless

type ~>[F[_], G[_]] = [A] => F[A] => G[A]
