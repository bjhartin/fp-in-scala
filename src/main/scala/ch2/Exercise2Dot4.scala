object Exercise2Dot4 extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length != 0)
      println("USAGE: Exercise2Dot4")
    else {
      val curried = (a: Int) => (b: Int) => a + b
      val uncurried = uncurry(curried)
      assert(uncurried(3,5) == 8)
    }
  }

  private[this] def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}
