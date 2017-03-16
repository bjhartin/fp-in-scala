object Exercise3Dot3 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(setHead(6, List(1,2,3,4,5)) == List(6,2,3,4,5), "Lists weren't equal")
    assert(setHead(1, Nil) == List(1), "Didn't work for Nil")
  }
}
