object Exercise3Dot13 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val result = foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))

      val as = List(1,2,3)
    val add = (x: Int, y: Int) => x + y
    val cons = (x: Int, y: List[Int]) => Cons(x,y)

    assert(foldRight2(as, 0)(add) == foldRight(as, 0)(add), "foldRight2 doesn't work")
    assert(foldRight2(as, Nil:List[Int])(cons) != foldRight(as, Nil:List[Int])(cons), "foldRight2 doesn't work")
  }
}
