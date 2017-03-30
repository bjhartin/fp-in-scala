package ch4

object Exercise4Dot2 extends App {
  import Stats._

  println(variance(Seq(1, 5, 20)))
  assert(variance(Nil) == None)
  assert(variance(Seq(1, 5, 20)) == Some(2.0086617119951207E7))
}
