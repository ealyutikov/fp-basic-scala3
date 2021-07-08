package core

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def apply2[A, B, C](fa: F[A], fb: F[B])(ff: F[(A, B) => C]): F[C] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a, b))))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E) =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c))

  def flip[A, B](ff: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(ff)

  def traverse[A,B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldRight(pure(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def productR[A, B](fa: F[A])(fb: F[B]): F[B] =
    apply(fb)(as(fa, { (b: B) => b }))

  def productL[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)

  extension [A, B] (fa: F[A])
    infix def *> (fb: F[B]): F[B] = productR(fa)(fb)
    infix def <* (fb: F[B]): F[A] = productL(fa)(fb)

object Applicative:
  def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[[X] =>> F[G[X]]] =
    new Applicative[[X] =>> F[G[X]]]:
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] =
        val x: F[G[A] => G[B]] = F.map(ff)(gab => G.flip(gab))
        F.apply(fga)(x)

  given Applicative[List] with
    def pure[A](a: A): List[A] = List(a)
    def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)

  given Applicative[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] =
      (fa, ff) match
        case (None, _) => None
        case (Some(a), None) => None
        case (Some(a), Some(f)) => Some(f(a))

trait ApplicativeLaws:

  def identity[F[_], A](fa: F[A])(using F: Applicative[F]): Boolean =
    F.apply(fa)(F.pure(a => a)) == fa

  def homomorphism[F[_], A, B](a: A, f: A => B)(using F: Applicative[F]): Boolean =
    F.apply(F.pure(a))(F.pure(f)) == F.pure(f(a))

  def interchange[F[_], A, B](a: A, ff: F[A => B])(using F: Applicative[F]): Boolean =
    F.apply(F.pure(a))(ff) == F.apply(ff)(F.pure(f => f(a)))

  def map[F[_], A, B](fa: F[A], f: A => B)(using F: Applicative[F]): Boolean =
    F.map(fa)(f) == F.apply(fa)(F.pure(f))