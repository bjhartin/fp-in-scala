package ch4

object Exercise4Dot5 extends App {
  import Option._

  val squareEvens = (a:Int) => if(a % 2 == 0) Some(a * a) else None

  assert(traverse(Nil)(squareEvens) == None)
  assert(traverse(List(1, 2))(squareEvens) == None)
  assert(traverse(List(2, 4))(squareEvens) == Some(List(4, 16)))

  assert(sequence2(Nil) == None)
  assert(sequence2(List(Some(1), None)) == None)
  assert(sequence2(List(Some(1), Some(2))) == Some(List(1, 2)))
}
