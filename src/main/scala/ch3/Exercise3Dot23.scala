object Exercise3Dot23 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(zipWith(List(1,2,3), List(4,5,6))(_ * _) == List(4, 10, 18), "zipWith doesn't work")
  }
}
