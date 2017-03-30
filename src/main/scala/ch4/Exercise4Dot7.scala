package ch4

object Exercise4Dot7 extends App {
  import Either._

  val squareEvens: Int => Either[String, Int] = (a:Int) => if(a % 2 == 0) Right(a * a) else Left(s"$a is not even")
  val toUpper = (s:String) => s.toUpperCase()

  assert(sequence(List(Left("oops"), Right("yay"))) == Left("oops"))
  assert(sequence(List(Left("oops"), Left("dang"), Right("yay"))) == Left("oops"))
  assert(sequence(List(Right("wow"), Right("yay"))) == Right(List("wow", "yay")))

  assert(traverse(List(1, 3, 2))(squareEvens) == Left("1 is not even"))
  assert(traverse(List(2, 4))(squareEvens) == Right(List(4, 16)))
}
