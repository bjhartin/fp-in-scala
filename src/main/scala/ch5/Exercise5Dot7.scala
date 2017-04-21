package ch5

object Exercise5Dot7 extends App {
  val x = Stream(2, 3, 4)

  assert(x.map(_ * 2) == Stream(4, 6, 8))
  assert(x.filter(_ % 2 == 0) == Stream(2, 4))
  assert(x.append(Stream(5)) == Stream(2, 3, 4, 5))
  assert(x.flatMap(a => Cons(() => a, () => Empty)) == Stream(2, 3, 4))
  assert(x.find(_ == 2) == Some(2))
}
