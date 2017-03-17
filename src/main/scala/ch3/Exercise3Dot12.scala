object Exercise3Dot12 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(reverse(List(1,2,3)) == List(3,2,1), "Reverse doesn't work")
    assert(reverse(Nil) == Nil, "Reverse fails for Nil")
  }
}
