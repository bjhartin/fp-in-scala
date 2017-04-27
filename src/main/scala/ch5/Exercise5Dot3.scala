package ch5

object Exercise5Dot3 extends App {
  import Stream._

  val x = Stream(1, 2, 3, 4)

  assert(x.takeWhile(_ <= 3) == Stream(1, 2, 3))

  //  What should it do for infinite streams?
  //
  //  If the prop is false soon enough, we'll be okay.
  //  If it's true long enough, we may get a stack overflow.

  // I cheat and use 'from' which is defined for a later exercise.
  assert(from(1).takeWhile(_ < 5) == x)
  assert(from(1).takeWhile(_ < 1) == Empty)
}
