package core

import data.{Id, NonEmptyList}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf

trait Comonad[F[_]] extends Functor[F]:
  def extract[A](fa: F[A]): A
  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    coflatMap(fa)(fa => f(extract(fa)))

  def coflatten[A](fa: F[A]): F[F[A]] =
    coflatMap(fa)(fa => fa)

object Comonad:
  given Comonad[Id] with
    def extract[A](fa: Id[A]): A = fa.extract
    def coflatMap[A, B](fa: Id[A])(f: Id[A] => B): Id[B] = fa.coflatMap(f)

  given Comonad[Future] with
    def extract[A](fa: Future[A]): A = Await.result(fa, Inf)
    def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))

  given Comonad[NonEmptyList] with
    def extract[A](fa: NonEmptyList[A]): A = ???
    def coflatMap[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] = ???

trait ComonadLaws:
  def extractCoflattenIdentity[F[_], A](fa: F[A])(using C: Comonad[F]) =
    C.extract(C.coflatten(fa)) == fa

  def mapCoflattenIdentity[F[_], A](fa: F[A])(using C: Comonad[F]) =
    C.map(C.coflatten(fa))(C.extract) == fa

  def mapCoflatMapCoherence[F[_], A, B](fa: F[A], f: A => B)(using C: Comonad[F]) =
    C.map(fa)(f) == C.coflatMap(fa)(fa0 => f(C.extract(fa0)))

  def comonadLeftIdentity[F[_], A](fa: F[A])(using C: Comonad[F]) =
    C.coflatMap(fa)(C.extract) == fa

  def comonadRightIdentity[F[_], A, B](fa: F[A], f: F[A] => B)(using C: Comonad[F]) =
    C.extract(C.coflatMap(fa)(f)) == f(fa)
