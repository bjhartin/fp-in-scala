package ch4

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.flatMap{(a:A) => ob}
  }


  def filter(f: A => Boolean): Option[A] = {
    this.flatMap {(a:A) => if(f(a)) Some(a) else None}
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
      as.foldRight(Some(Nil): Option[List[A]]) {
        case (Some(elem), Some(acc)) => Some(elem :: acc)
        case _ => None
      }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(Nil): Option[List[B]]) {(acc, a) =>
      (f(a), acc) match {
        case (Some(b), Some(bs)) => Some(b :: bs)
        case _ => None
      }
    }.map(_.reverse) // Ugh.
}