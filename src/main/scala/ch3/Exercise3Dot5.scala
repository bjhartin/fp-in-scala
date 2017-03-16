object Exercise3Dot5 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(dropWhile(List(1,2,3,4,5), (i:Int) => i < 4) == List(4,5), "Lists weren't equal")
    assert(dropWhile(List(1,2,3,4,5), (i:Int) => i < 6) == Nil, "Didn't handle function that matched all elements")
    assert(dropWhile(Nil, (i:Int) => i < 6) == Nil, "Didn't work for Nil")
  }
}
