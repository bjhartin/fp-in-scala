import scala.collection.immutable.{List => ScalaList}

object Exercise2Dot1 extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length != 1)
      println("USAGE: Exercise2Dot1 N (where N is which fibonacci number is desired, e.g. 4th)")
    else {
      val i = args(0).toInt
      println(s"Fibonacci number $i is ${fibonacci(i)}")
    }
  }


  @annotation.tailrec
  private[this] def fibonacci(i: Int, fibs: ScalaList[Int] = ScalaList(1,0)): Int = {
    if(fibs.length == i)
      fibs.head
    else
      fibonacci(i, fibs(0) + fibs(1) :: fibs)
  }
 
  // Non-tail recursive, naive impl.
  // private[this] def fibonacci(i: Int): Int = {
  //   i match {
  //     case 1 => 1
  //     case 0 => 0
  //     case other => fibonacci(other - 2) + fibonacci(other - 1)
  //   }
  // }
}
