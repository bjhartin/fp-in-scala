package ch5

object Section5Dot4Examples extends App {
  import Stream._

  assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
  assert(ones.exists(_ % 2 != 0))
  assert(ones.map(_ + 1).exists(_ % 2 == 0))

  val s = ones.takeWhile(_ == 1)
  // This would not terminate because it evaluates the stream
  // assert(s == ones)

  assert(!ones.forAll(_ != 1))
}
