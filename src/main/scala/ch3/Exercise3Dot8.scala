object Exercise3Dot8 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val result = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    assert(result == List(1,2,3))

    /*
     Question: What do you think this
     says about the relationship between foldRight and the data constructors of List?

     Answer: They are both recursive.

     Let's look at what happens when we call foldRight(List(1,2,3),0)(_+_)


     case Cons(x, xs) =>           // x == 1, xs == List(2,3)
      f(x, foldRight(xs, z)(f))

      foldRight(List(1,2,3),0)(f))
      f(1, foldRight(List(2,3),0)(f))
      f(1, f(2, foldRight(List(3),0)(f)))
      f(1, f(2, f(3, foldRight(Nil, 0)(f))))
      f(1, f(2, f(3, 0)))
      1 + (2 + (3 + 0)))
      6

      As we recurse down, the provided function is applied to the head and the result of foldRight on
      the tail.

      This implies a couple things about f.

      1. f has the signature (A,B) => B, because it's working on a List[A].
        1.1. Of course, A may be equal to B, and often will be (Semigroups)
      2. f is applied in a right-associative manner, e.g. summing (1,2,3) using
      foldRight will give us 1 + (2 + 3).

     */
  }
}
