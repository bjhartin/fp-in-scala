package ch5

object Exercise5Dot4 extends App {
  assert(! Cons(() => 1, () => Cons(() => 2, () => Empty)).forAll(_ % 2 == 0))

  assert(Cons(() => 2, () => Cons(() => 4, () => Empty)).forAll(_ % 2 == 0))

  assert(Empty.forAll(_ => false))

  assert(Empty.forAll(_ => true))
}
