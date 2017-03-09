object Exercise2Dot3 extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length != 0)
      println("USAGE: Exercise2Dot3")
    else {
      val curried = curry((x: Int, y: Int) => x + y)
      assert(curried(3)(5) == 8)
    }
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }
}
