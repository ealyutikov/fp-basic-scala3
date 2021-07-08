package data

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(left, rught) => 1 + left.size + rught.size

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), Branch(_,_))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))