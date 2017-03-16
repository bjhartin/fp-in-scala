object Exercise3Dot11 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(sum3(List(1,2,3)) == 6, "Sum with foldLeft doesn't work")
    assert(product3(List(1,2,3,4)) == 24, "Product with foldLeft doesn't work")
  }
}
