package optic

trait Optional[S, A] { self =>
  def modify(f: A => A): S => S
  def set(a: A): S => S = modify(_ => a)
  def getOption(s: S): Option[A]

  def combine[B](optional: Optional[A, B]): Optional[S, B] =
    new Optional[S, B]:
      def modify(f: B => B): S => S =
        (optional.modify _).andThen(self.modify)(f)
      def getOption(s: S): Option[B] =
        self.getOption(s).flatMap(optional.getOption)

  def combine[B](lens: Lens[A, B]): Optional[S, B] =
    new Optional[S, B]:
      def modify(f: B => B): S => S =
        (lens.modify _).andThen(self.modify)(f)
      def getOption(s: S): Option[B] =
        self.getOption(s).map(lens.get)

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
        self.getOption(s).flatMap(prism.getOption)
}
