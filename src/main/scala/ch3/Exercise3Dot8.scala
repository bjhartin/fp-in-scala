object Exercise3Dot8 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val result = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    assert(result == List(1,2,3))

    // Question: What do you think this
    // says about the relationship between foldRight and the data constructors of List?

    // Answer: They are both recursive.
  }
}
