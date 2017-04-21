package ch5

object Exercise5Dot9 extends App {
  import Stream._

  assert(from(1).take(5) == Stream(1, 2, 3, 4, 5))
}
