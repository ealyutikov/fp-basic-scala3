package data

import core.{Applicative, Functor}

final case class Id[A](a: A):
  def map[B](f: A => B): Id[B] = Id(f(a))
  def flatMap[B](f: A => Id[B]): Id[B] = f(a)

  def extract: A = a
  def coflatten: Id[Id[A]] = Id(this)
  def coflatMap[B](f: Id[A] => B): Id[B] = coflatten.map(f)

object Id:
  given Functor[Id] with
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = fa.map(f)

  given Applicative[Id] with
    def pure[A](a: A): Id[A] = Id(a)
    def apply[A, B](fa: Id[A])(ff: Id[A => B]): Id[B] = fa.map(ff.a)
