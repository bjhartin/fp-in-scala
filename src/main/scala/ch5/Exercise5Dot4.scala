package ch5

object Exercise5Dot4 extends App {
  assert(! Stream(1, 2).forAll(_ % 2 == 0))

  assert(Stream(2, 4).forAll(_ % 2 == 0))

  assert(Empty.forAll(_ => false))

  assert(Empty.forAll(_ => true))
}
