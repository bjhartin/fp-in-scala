package ch5

object Section5Dot1Examples extends App {
  var counter = 0
  val expensive = () => {
    counter += 1
    1
  }

  val stream1 = Cons(() => expensive(), () => Empty: Stream[Int])
  assert(counter == 0)
  stream1.h
  assert(counter == 0)
  stream1.h()
  assert(counter == 1)
  stream1.h()
  assert(counter == 2)

  counter = 0

  // Using Stream.cons uses lazy vals so calls to h() and t()
  // don't re-evaluate the original by-name values.
  val stream2 = Stream.cons(expensive(), Empty: Stream[Int]).asInstanceOf[Cons[Int]]
  assert(counter == 0)
  stream2.h
  assert(counter == 0)
  stream2.h()
  assert(counter == 1)
  stream2.h()
  assert(counter == 1)
}
