package ch4

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = flatMap(a => Success(f(a)))

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = this match {
    case Success(a) => f(a)
    case l@Failure(e) => l // Compiler can't guess type for 'case l => l'
  }

  def orElse[EE >: E,B >: A](b: => Validation[EE, B]): Validation[EE, B] = this match {
    case Failure(e) => b
    case r => r
  }

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = flatMap(a => b.map(f(a,_)))
}

case class Failure[+E](errors: List[E]) extends Validation[E, Nothing]
case class Success[+A](value: A) extends Validation[Nothing, A]

object Validation {
  def sequence[E, A](as: List[Validation[E, A]]): Validation[E, List[A]] = traverse(as)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
    as.foldLeft(Success(Nil): Validation[E, List[B]]) {(acc, a) =>
      (f(a), acc) match {
        case (Success(b), Success(bs)) => Success(b :: bs)
        case (f@Failure(es), Success(bs)) => f // First error encountered
        case (Success(b), f@Failure(es)) => f // Preserves existing error
        case (Failure(es1), Failure(es2)) => Failure(es2 ::: es1) // Accumulates errors
      }
    }.map(_.reverse) // Ugh.
}