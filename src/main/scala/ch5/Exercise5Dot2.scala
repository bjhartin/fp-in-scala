package ch5

object Exercise5Dot2 extends App {
  val x = Stream(1, 2, 3, 4, 5)

  assert(x.take(2) == Stream(1, 2))

  assert(x.drop(2) == Stream(3, 4, 5))

  //  What does it do for infinite streams?
  //  Works fine.
  //  import Stream._
  //  assert(ones.take(2).equals(Stream(1, 1)))
}
