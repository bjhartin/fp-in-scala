package ch5

object Exercise5Dot6 extends App {
  val x = Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Empty)))

  assert(x.headOption == Some(2))
  assert(Empty.headOption == None)
}
