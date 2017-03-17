object Exercise3Dot17 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(toStrings(List(1.0,2.0,3.0)) == List("1.0","2.0","3.0"), "toStrings doesn't work")
  }
}
