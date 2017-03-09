object Exercise2Dot5 extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length != 0)
      println("USAGE: Exercise2Dot5")
    else {
      val newFunc = compose((b: Int) => b - 5, (a: Int) => a * 2)

      assert(newFunc(10) == 15)
    }
  }

  private[this] def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
