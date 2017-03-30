package ch4

import scala.List

object Exercise4Dot4 extends App {
  import Option._

  // Fixing A as Int makes it easier for me to run my test on different implementations
  // of sequence.
  def check[A](seq: List[Option[A]] => Option[List[A]], exampleAs: Tuple2[A,A]): Unit = {
    assert(seq(Nil) == None)
    assert(seq(List(Some(exampleAs._1), None)) == None)
    assert(seq(List(Some(exampleAs._1), Some(exampleAs._2))) == Some(List(1, 2)))
  }

  check(sequence[Int], (1,2))
}
