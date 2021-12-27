package optic

trait Lens[S, A] { self =>
  def modify(f: A => A): S => S
  def get(s: S): A
  def set(a: A): S => S = modify(_ => a)

  def combine[B](lens: Lens[A, B]): Lens[S, B] =
    new Lens[S, B]:
      def modify(f: B => B): S => S =
        lens.modify.andThen(self.modify)(f)
      def get(s: S): B =
        self.get.andThen(lens.get)(s)

  def combine[B](prism: Prism[A, B]): Optional[S, B] =
    new Optional[S, B]:
      def modify(f: B => B): S => S =
        self.modify { a =>
          prism
            .getOption(a)
            .map(f)
            .map(prism.reverseGet)
            .getOrElse(a)
        }
      def getOption(s: S): Option[B] =
        self.get.andThen(prism.getOption)(s)
}



