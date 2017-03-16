object Exercise3Dot4 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(drop(3, List(1,2,3,4,5)) == List(4,5), "Lists weren't equal")
    assert(drop(5, List(1,2,3)) == Nil, "Didn't work when count too big")
    assert(drop(1, Nil) == Nil, "Didn't work for Nil")
  }
}
