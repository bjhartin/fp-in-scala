object Exercise3Dot13 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val result = foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))

    /*

      I don't think this question is really worded properly, because
      the solution given at https://github.com/astorije/fpinscala-exercises/blob/master/src/main/scala/ch3datastructures/List.scala
      only works in some cases.  If the reduction preserves any information about the original
      order, it fails.  This seems related to commutativity, but not quite?

      Cons(2, Cons(1, Nil)) != Cons(1, Cons(2, Nil))

      2 + (1 + Nil) != 1 + (2 + Nil) (where + == Cons)

      If we curry, things might be clearer:

      Cons(2)(Cons(1)(Nil)) != Cons(1)(Cons(2)(Nil))  // Cons(2) and Cons(1) are now List[A] => B

      f(g(Nil)) != g(f(Nil))

     */

    val as = List(1,2,3)
    val add = (x: Int, y: Int) => x + y
    val sub = (x: Int, y: Int) => x - y
    val cons = (x: Int, y: List[Int]) => Cons(x,y)

    assert(foldRight2(as, 0)(add) == foldRight(as, 0)(add), "foldRight2 doesn't work")

    // This will fail.
    // Cons(1, Cons(2, Cons(3, Nil))) != Cons(3, Cons(2, Cons(1, Nil)))
    assert(foldRight2(as, Nil:List[Int])(cons) != foldRight(as, Nil:List[Int])(cons), "foldRight2 doesn't work")


  }
}
