package extension

import core.Monoid

object ListExt:
  extension [A](xs: List[A])
    def collectWithM[B](f: A => B)(using B: Monoid[B]): B =
      xs.view.map(f).foldLeft(B.empty)(B.combine)

    def collectM(using Monoid[A]): A =
      collectWithM(x => x)
