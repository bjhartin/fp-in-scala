package ch5

sealed trait Stream[+A] {
  def headOption: Option[A] = foldRight(None: Option[A])((a, acc) => Some(a))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def exists(p: A => Boolean) = foldRight(false)((a, acc) => p(a) || acc)

  def forAll(p: A => Boolean) = foldRight(true)((a, acc) => p(a) && acc)

  def find(p: A => Boolean): Option[A] = foldRight(Option.empty[A]) ((a, acc) => if (p(a)) Some(a) else None)

  def takeWhile(p: A => Boolean) = foldRight(Empty: Stream[A])((a, acc) =>
    if (p(a)) Cons(() => a, () => acc) else Empty)

  /*

    Takewhile works.  Consider Stream.from(1).takeWhile(_ < 5).

    When we get to 4, we have this in foldRight:

    (Might help to read 'Stream' as the equivalent 'Cons')

      f(1, Stream.from(2).foldRight(Empty)(f))
      f(1, f(2, Stream.from(3).foldRight(Empty)(f)))
      f(1, f(2, f(3, Stream.from(4).foldRight(Empty)(f))))
      f(1, f(2, f(3, f(4, Stream.from(5).foldRight(Empty)(f)))))
      f(1, f(2, f(3, f(4, Empty))))  // Because p(5) == false
      f(1, f(2, f(3, Stream(4)))
      f(1, f(2, Stream(3, 4)))
      f(1, Stream(2, 3, 4)))
      Stream(1, 2, 3, 4)
   */

  def take(n: Int): Stream[A] = (n, this) match {
    case (x, Cons(h, t)) if x > 0 => Cons(h, () => t().take(n-1))
    case _ => Empty
  }

  def take2(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (x, Cons(h, t)) if x > 0 => Some(h(), (x - 1, t()))
    case _ => None
  }

  def takeWhile2(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) => {
      lazy val head = h()
      if(p(head)) Some(head, t()) else None
    }
    case _ => None
  }

  // To discuss: This is just an expansion of takeByPos.
  // It doesn't work, and I don't see why.

  //    foldRight((n, Empty: Stream[A]))((a, acc) =>
  //    if(acc._1 > 0){
  //      println(s"Keeping ${a}")
  //      (acc._1 - 1, Cons(() => a, () => acc._2))
  //    } else {
  //      println(s"Dropping ${a}")
  //      acc
  //    })._2

  //takeByPos(_ <= n)

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, s) => s
    case (_, Cons(t, h)) => h().drop(n-1)
    case _ => Empty
  }
  // /takeByPos(_ > n)

  def filter(p: A => Boolean) = foldRight(Empty: Stream[A])((a, acc) =>
    if (p(a)) Cons(() => a, () => acc) else acc
  )

  def append[B >: A](s2: => Stream[B]): Stream[B] = {
    this.foldRight(s2)((b, acc) => Cons(() => b, () => acc))
  }

  // Reverses the order!  Why?
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, acc) => f(a).append(acc))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]){(a, acc) => Cons(() => f(a), () => acc)}

  def map2[B](f: A => B): Stream[B] = Stream.map2(this)(f)

  def startsWith[A](possiblePrefix: Stream[A]): Boolean = {
    Stream.zipWith(this, possiblePrefix)(_ == _) == possiblePrefix.map(_ => true)
  }

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

  override def equals(other: Any) = other match {
    case that: Cons[A] => this.h() == that.h() && this.t() == that.t()
    case _ => false
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

  def foldRight[A, B](s: () => Stream[A], b: B)(f: (A, => B) => B): B = {
    s() match {
      case Empty => b
      case Cons(h, t) => f(h(), foldRight(t, b)(f))
    }
  }

  def ones: Stream[Int] = constant(1) // Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fibGen(n: Int, m: Int): Stream[Int] = cons(m, fibGen(m, n + m))
    fibGen(0, 1)
  }

  def map2[A,B](as: Stream[A])(f: A => B): Stream[B] = {
    // The state is the original Stream.
    // The generation comes from f.
    // When (if) the original stream terminates, we're done.
    unfold(as) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))

  def from2(n: Int): Stream[Int] = {
    def add(i: Int) = Some(i, i + 1)
    unfold(n)(add)
  }

  def fibs2(): Stream[Int] = {
    def fibs(t: (Int, Int)): Option[(Int, (Int, Int))] = Some((t._2, (t._2, t._1 + t._2)))
    unfold((0,1))(fibs)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  def zipWith[A,B](a1: Stream[A], a2: Stream[A])(f: (A,A) => B) : Stream[B] =
    unfold((a1, a2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(f(x1(), x2()), (xs1(), xs2()))
    }

  def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((s1, s2)) {
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(((Some(x1()), Some(x2())), (xs1(), xs2())))
      case (Cons(x1, xs1), Empty) => Some(((Some(x1()), None), (xs1(), Empty)))
      case (Empty, Cons(x2, xs2)) => Some(((None, Some(x2())), (Empty, xs2())))
      case (Empty, Empty) => None
    }

  def startsWith[A](s1: Stream[A], possiblePrefix: Stream[A]): Boolean = {
    zipWith(s1, possiblePrefix)(_ == _) == possiblePrefix.map(_ => true)
  }
}