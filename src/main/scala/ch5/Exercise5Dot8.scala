package ch5

object Exercise5Dot8 extends App {
  import Stream._

  assert(constant(5).take(3) == Stream(5, 5, 5))
}
