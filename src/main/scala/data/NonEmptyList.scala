package data

final case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = head :: tail

  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def flatMap[B >: A](f: A => NonEmptyList[B]): NonEmptyList[B] =
    val result = f(head)
    NonEmptyList(result.head, result.tail ::: tail.flatMap(a => f(a).toList))

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    tail.foldLeft(f(b, head))(f)
