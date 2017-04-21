package ch5

object Exercise5Dot3 extends App {
  val x = Stream(1, 2, 3, 4)

  assert(x.takeWhile(_ <= 3) == Stream(1, 2, 3))

  //  What should it do for infinite streams?
  //
  //  If the prop is false soon enough, we'll be okay.
  //  If it's true long enough, we may get a stack overflow.
  //  import Stream._
  //  assert(ones.takeWhile(_ > 1) == Empty)
}
