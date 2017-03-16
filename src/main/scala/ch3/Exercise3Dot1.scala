object Exercise3Dot1 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val result = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(s"Result is $result") // expect 3
  }
}
