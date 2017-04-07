package ch4

object Exercise4Dot8 extends App {
  import Validation._

  val squareEvens: Int => Validation[String, Int] = (a:Int) => if(a % 2 == 0) Success(a * a) else Failure(List(s"$a is not even"))

  assert(traverse(List(1, 3, 2))(squareEvens) == Failure(List("1 is not even", "3 is not even")))
  assert(traverse(List(2, 4))(squareEvens) == Success(List(4, 16)))
}
