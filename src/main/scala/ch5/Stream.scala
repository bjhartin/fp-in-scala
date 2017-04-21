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
    if (p(a)) Cons(() => a, () => acc) else acc)

  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => Empty
    case (_, Cons(t, h)) => Cons(t, () => h().take(n-1))
    case _ => Empty
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

  // Reverses the order!
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, acc) => f(a).append(acc))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]){(a, acc) => Cons(() => f(a), () => acc)}

  def map2[B](f: A => B): Stream[B] = Stream.map2(this)(f)

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
}