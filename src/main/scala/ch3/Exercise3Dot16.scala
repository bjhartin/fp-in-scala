object Exercise3Dot16 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(addOneToAll(List(1,2,3)) == List(2,3,4), "addOneToAll doesn't work")
  }
}
