package ch5

sealed trait Stream[+A] {
//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => Some(h())
//  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, acc) => Some(a))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def exists(p: A => Boolean) = foldRight(false)((a, acc) => p(a) || acc)

  def forAll(p: A => Boolean) = foldRight(true)((a, acc) => p(a) && acc)

  def takeWhile(p: A => Boolean) = foldRight(Empty: Stream[A])((a, acc) =>
    if (p(a)) Cons(() => a, () => acc) else acc
  )

  def take(n: Int): Stream[A] = takeByPos(_ <= n)

  def drop(n: Int): Stream[A] = takeByPos(_ > n)

  def filter(p: A => Boolean) = foldRight(Empty: Stream[A])((a, acc) =>
    if (p(a)) Cons(() => a, () => acc) else acc
  )

  def append[B >: A](s2: Stream[B]): Stream[B] = {
    this.foldRight(s2)((b, acc) => Cons(() => b, () => acc))
  }

  // Reverses the order!
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, acc) => f(a).append(acc))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]){(a, acc) => Cons(() => f(a), () => acc)}

  protected def foldRight[B](b: B)(f: (A, => B) => B): B = this match {
    case Empty => b
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
  }

  private def takeByPos(p: Int => Boolean): Stream[A] = foldRight((0, Empty: Stream[A]))((a, acc) =>
    if(p(acc._1)) (acc._1 + 1, Cons(() => a, () => acc._2)) else acc)._2
}

case object Empty extends Stream[Nothing] {
  override def toString: String = "Empty"
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString: String = {
    s"Cons(${h()}, ${t().toString})"
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}