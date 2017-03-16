object Exercise3Dot9 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(length(List(1,2,3)) == 3, "Length doesn't work")
    assert(length(Nil) == 0, "Length doesn't work for Nil")
  }
}
