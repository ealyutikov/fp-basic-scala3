package tagless

type FunctionK[F[_], G[_]] = [A] => F[A] => G[A]

object FunctionK:
  type ~>[F[_], G[_]] = FunctionK[F, G]

  val optionToList: Option ~> List = [A] => (opt: Option[A]) => opt.toList
  val listToOption: List ~> Option = [A] => (list: List[A]) => list.headOption
