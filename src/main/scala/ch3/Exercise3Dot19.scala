object Exercise3Dot19 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    val isOdd = (x: Int) => x % 2 != 0

    assert(filter(List(1,2,3))(isOdd) == List(1,3), "filter doesn't work")
  }
}
