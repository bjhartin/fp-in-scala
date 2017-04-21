package ch5

object Exercise5Dot12 extends App {
  import Stream._

  assert(constant2(5).take(3) == Stream(5, 5, 5))
  assert(from2(1).take(5) == Stream(1, 2, 3, 4, 5))
  assert(fibs2().take(5) == Stream(1, 1, 2, 3, 5))
}
