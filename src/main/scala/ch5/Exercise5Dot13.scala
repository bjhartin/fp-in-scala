package ch5

object Exercise5Dot13 extends App {
  import Stream._

  assert(from(1).take(3).map(_ * 2) == Stream(2, 4, 6))
  assert(from(1).take(3).map2(_ * 2) == Stream(2, 4, 6))

  // TODO: take, takeWhile, zipWith, zipAll
  
}
