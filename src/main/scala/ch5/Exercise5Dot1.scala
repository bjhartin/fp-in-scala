package ch5

object Exercise5Dot1 extends App {
  val x = Cons(() => 2, () => Cons(() => 3, () => Empty))

  assert(x.toList == List(2, 3))

  //  What does it do for infinite streams?
  //  StackOverflowError, as you'd expect:

  //  import Stream._
  //  ones.toList
}
