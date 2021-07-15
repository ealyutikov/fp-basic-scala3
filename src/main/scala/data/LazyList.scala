package data

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(head: () => A, tail: () => LazyList[A])

  def foldRight[B](zero: => B)(f: (A, => B) => B): B =
    this match
      case Cons(head, tail) => f(head(), tail().foldRight(zero)(f))
      case _                => zero

  def toList: List[A] =
    @tailrec
    def loop(list: LazyList[A], acc: List[A]): List[A] =
      list match
        case Cons(head, tail) => loop(tail(), head() :: acc)
        case _                => acc

    loop(this, List()).reverse
