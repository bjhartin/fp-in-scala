object Exercise3Dot15 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(flatten(List(List(1,2),List(3,4))) == List(1,2,3,4), "Flatten doesn't work")

    /*

    Question: Is runtime linear in the total length of all lists?

    Answer: Not as written.  Can we improve?

     */
  }
}
