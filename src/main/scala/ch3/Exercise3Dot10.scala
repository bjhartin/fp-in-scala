object Exercise3Dot10 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(foldLeft(List(1,2,3),0)(_ + _) == 6, "fold left doesn't work")
    assert(foldLeft(List(0),0)(_ + _) == 0, "fold left doesn't work")

    /*
    Let's look at what happens when we call foldLeft(List(1,2,3),0)(_+_)


     case Cons(x, xs) =>           // x == 1, xs == List(2,3)
      foldLeft(xs, f(z,x))(f)

      foldLeft(List(1,2,3),0)(f)
      foldLeft(List(2,3), f(0,1))(f)    // f(0, 1)
      foldLeft(List(3), f(1,2))(f)      // f(f(0, 1), 2) == f(1, 2)
      foldLeft(Nil, f(3,3))(f)          // f(f(f(0, 1), 2), 3) == f(f(1, 2), 3) == f(3, 3)
      ((0 + 1) + 2) + 3
      6

      As we recurse down, the provided function is applied to the accumulator and the head.

      This implies a couple things about f.

      1. f has the signature (B,A) => B.  Note the difference with foldRight.
      2. f is applied in a left-associative manner, e.g. summing (1,2,3) using
      foldLeft will give us (1 + 2) + 3.
     */
  }
}
