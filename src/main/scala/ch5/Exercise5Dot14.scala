package ch5

object Exercise5Dot14 extends App {
  import Stream._

  assert(Stream(1,2,3,4) startsWith Stream(1,2))
  assert(!(Stream(1,2,3,4) startsWith Stream(1,2,5)))
  assert(from(1) startsWith Stream(1,2,3))
  assert(!(from(1) startsWith Stream(1,2,4)))

  // Of course, this doesn't terminate
  //assert(from(1) startsWith from(1))
}
