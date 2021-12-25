package optic

trait Setter[S, A]:
  def modify(f: A => A): S => S
  def set(a: A): S => S = modify(_ => a)

  def compose[B](setter: Setter[A, B]): Setter[S, B] =
    (f: B => B) => setter.modify.andThen(this.modify)(f)
