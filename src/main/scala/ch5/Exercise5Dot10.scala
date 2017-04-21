package ch5

object Exercise5Dot10 extends App {
  import Stream._

  assert(fibs().take(5) == Stream(1, 1, 2, 3, 5))
}
