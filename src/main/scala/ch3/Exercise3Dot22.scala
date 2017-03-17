object Exercise3Dot22 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(addPairs(List(1,2,3), List(4, 5, 6)) == List(5, 7, 9), "addPairs doesn't work")
  }
}
