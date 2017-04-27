package ch5

object Exercise5Dot13 extends App {
  import Stream._

  assert(from(1).take(3).map(_ * 2) == Stream(2, 4, 6))
  assert(from(1).take(3).map2(_ * 2) == Stream(2, 4, 6))

  assert(from(1).take(3) == Stream(1, 2, 3))
  assert(from(1).take2(3) == Stream(1, 2, 3))

  // Something wrong with 'take' in that it doesn't terminate for infinite streams.
  assert(from(1).takeWhile(_ <= 3) == Stream(1, 2, 3))
  assert(from(1).takeWhile2(_ <= 3) == Stream(1, 2, 3))

  assert(zipWith(from(1), from(1))(_ * _).takeWhile(_ < 20) == Stream(1, 4, 9, 16))
  assert(zipAll(Stream(1, 2), Stream(1)) == Stream((Some(1), Some(1)), (Some(2), None)))
  assert(zipAll(Stream(1), Stream(1, 2)) == Stream((Some(1), Some(1)), (None, Some(2))))
}
