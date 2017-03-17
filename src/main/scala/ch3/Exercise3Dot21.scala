object Exercise3Dot21 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val isOdd = (x: Int) => x % 2 != 0

    assert(filter2(List(1,2,3))(isOdd) == List(1,3), "filter doesn't work")
  }
}
