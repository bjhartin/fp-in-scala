object Exercise3Dot14 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(append(List(1,2,3), List(4, 5)) == List(1,2,3,4,5), "Append doesn't work")
  }
}
