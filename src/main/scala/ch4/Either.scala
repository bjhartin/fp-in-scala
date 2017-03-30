package ch4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l@Left(e) => l // Compiler can't guess type for 'case l => l'
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case r => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(a => b.map(f(a,_)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(Right(Nil): Either[E, List[B]]) {(acc, a) =>
      (f(a), acc) match {
        case (Right(b), Right(bs)) => Right(b :: bs)
        case (l@Left(e), Right(bs)) => l
        case (_, l@Left(e)) => l // Necessary to preserve the first error
      }
    }.map(_.reverse) // Ugh.
}