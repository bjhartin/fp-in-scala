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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, Nil) => Cons(x, Nil)
    case Cons(x1, Cons(x2, Nil)) => Cons(x2, Cons(x1, Nil))
  }


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