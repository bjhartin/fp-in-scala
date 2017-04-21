package ch5

object Exploration extends App {
  val x = Stream(4, 9, 12, 13)

//  println(x.foldRight(Nil: List[Int])((a, acc) => a :: acc))
//
//  println(x.foldRight(Empty: Stream[Int])((a, acc) => if(a % 2 == 0) Cons(() => a, () => acc) else acc))
//
//  println(x.foldRight(Empty: Stream[Int])((a, acc) => Cons(() => a, () => acc)))

    println(x.drop(2))
}
