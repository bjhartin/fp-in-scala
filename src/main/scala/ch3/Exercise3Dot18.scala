object Exercise3Dot18 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(map(List(1,2,3))(_ * 2) == List(2, 4, 6), "Map doesn't work")
  }
}
