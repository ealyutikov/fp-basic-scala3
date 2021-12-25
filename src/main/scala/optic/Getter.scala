package optic

trait Getter[S, A]:
  def get(s: S): A
  def andThen[B](getter: Getter[A, B]): Getter[S, B] =
    (s: S) => this.get.andThen(getter.get)(s)

object Getter:
  def apply[S, A](f: S => A): Getter[S, A] =
    (s: S) => f(s)
