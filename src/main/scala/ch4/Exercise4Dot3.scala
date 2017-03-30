package ch4

object Exercise4Dot3 extends App {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      val1 <- a
      val2 <- b
    } yield f(val1, val2)
  }

  assert(map2(None, None)((_, _)) == None)
  assert(map2(Some(1), None)((_, _)) == None)
  assert(map2(None, Some(2))((_, _)) == None)
  assert(map2(Some(1), Some(2))((_, _)) == Some((1, 2)))
}
