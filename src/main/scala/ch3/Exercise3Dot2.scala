object Exercise3Dot2 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(tail(List(1,2,3,4,5)) == List(2,3,4,5), "Lists weren't equal")
    assert(tail(Nil) == Nil, "Didn't work for Nil")

    // Question: What are different choices you could make in your
    // implementation if the List is Nil?

    // Answer: Not sure.
  }
}
