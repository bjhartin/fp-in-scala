package ch5

object Exercise5Dot5 extends App {
  val x = Cons(() => 2, () => Cons(() => 3, () => Empty)).takeWhile(_ % 2 == 0)
  val y = Cons(() => 2, () => Empty)

  assert(x.toString == y.toString)
}
