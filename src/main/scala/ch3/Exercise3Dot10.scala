object Exercise3Dot10 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(foldLeft(List(1,2,3),0)(_ + _) == 6, "fold left doesn't work")
    assert(foldLeft(List(0),0)(_ + _) == 0, "fold left doesn't work")
  }
}
