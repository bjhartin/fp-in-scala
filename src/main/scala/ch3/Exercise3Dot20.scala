object Exercise3Dot20 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(flatMap(List(1,2,3))((i:Int) => List(i,i)) == List(1,1,2,2,3,3), "FlatMap didn't work")
  }
}
