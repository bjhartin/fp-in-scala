package ch4

object Exercise4Dot6 extends App {

  val toUpper = (s:String) => s.toUpperCase()

  val squareEvens: Int => Either[String, Int] = (a:Int) => if(a % 2 == 0) Right(a * a) else Left("Not even")

  assert(Left("oops").map(toUpper) == Left("oops"))
  assert(Right("yay").map(toUpper) == Right("YAY"))

  assert(Left("oops").flatMap(squareEvens) == Left("oops"))
  assert(Right(1).flatMap(squareEvens) == Left("Not even"))
  assert(Right(2).flatMap(squareEvens) == Right(4))

  assert(Left("oops").orElse(Right("yay")) == Right("yay"))
  assert(Right("yay").orElse(Right("wow")) == Right("yay"))

  assert(Left("oops").map2(Right("yay"))((a,b) => s"$a$b") == Left("oops"))
  assert(Right("wow").map2(Right("yay"))((a,b) => s"$a$b") == Right("wowyay"))
}
