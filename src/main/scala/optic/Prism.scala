package optic

trait Prism[S, A]:
  def reverseGet(a: A): S
  def getOption(s: S): Option[A]
  def modifyOption(f: A => A): S => Option[S] =
    s => getOption(s).map(f).map(reverseGet)

object Prism:
  def apply[S, A](pf: PartialFunction[S, A])(f: A => S): Prism[S, A] =
    new Prism[S, A]:
      def reverseGet(a: A): S = f(a)
      def getOption(s: S): Option[A] =
        Option(s).collect(pf)
