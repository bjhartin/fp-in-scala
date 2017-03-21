sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) =>
        f(x, foldRight(xs, z)(f))
    }
  }

  // Got help on this one.
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a,b))

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) =>
        foldLeft(xs, f(z,x))(f)
    }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, acc) => if(f(x)) Cons(x, acc) else acc)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else Nil)

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, acc) => Cons(x, acc))
  }

  def flatten[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])((list, acc) => append(list, acc))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def addPairs(a1: List[Int], a2: List[Int]): List[Int] =
    zipWith(a1, a2)(_ + _)

  def zipWith[A,B](a1: List[A], a2: List[A])(f: (A,A) => B) : List[B] = (a1, a2) match {
    // Ignoring edge cases for now
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1,x2), zipWith(xs1, xs2)(f))
  }

  def hasSubsequence[A](seq: List[A], subseq: List[A]): Boolean = {
    /*

      Explanation of how this works:

      First, a slight bit of syntax explanation.  I'm invoking foldLeft with a
      a 'pattern matching anonymous function'.  It's just a syntax niceity, no
      semantic difference.

      What this does is try to match the current element with the first element
      in the subsequence.  If it does not match, we try again with the next element
      by recursing.  If it does match, we try again with the next element and
      the *next element in the subsequence* by recursing *and dropping the head* of the subsequence.

      If you try this imperatively with 'for' loops, arrays and two indices seq_ind and subseq_ind,
      this step is equivalent to advancing subseq_ind on a match.
     */


    Nil == foldLeft(seq, subseq) {
      case (Cons(expected, remaining), current) =>
        if(current == expected) remaining else subseq  // Starts over if a partial subseq match fails
      case (Nil, _) => Nil
    }
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def addOneToAll(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def toStrings(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def setHead[A](newHead: A, as: List[A]): List[A] = as match {
    case Nil => Cons(newHead, Nil)
    case Cons(x,xs) => Cons(newHead, xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head,apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def drop[A](count: Int, as: List[A]): List[A] = as match {
    case Nil => Nil
    case _ =>
      if(count == 0)
        as
      else
        drop(count -1, tail(as))
  }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) =>
      if(f(x))
        dropWhile(xs, f)
      else
        as
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x1, Nil) => Nil
    case Cons(x1, xs) => Cons(x1, init(xs))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x,acc) => acc + 1)
}