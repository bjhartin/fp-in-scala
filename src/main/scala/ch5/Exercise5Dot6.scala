package ch5

object Exercise5Dot6 extends App {
  val x = Stream(2, 3, 4)

  assert(x.headOption == Some(2))
  assert(Empty.headOption == None)
}
