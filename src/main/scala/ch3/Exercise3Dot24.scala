object Exercise3Dot24 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(hasSubsequence(List(1,2,3),List(1,2)), "hasSubsequence doesn't work")

    assert(hasSubsequence(List(1,2,3),List(2,3)), "hasSubsequence doesn't work")

    assert(hasSubsequence(List(1,2,3,4),List(2,3)), "hasSubsequence doesn't work")

    assert(hasSubsequence(List(1),List(1)), "hasSubsequence doesn't work")

    assert(!hasSubsequence(List(1),List(2)), "hasSubsequence doesn't work")

    assert(!hasSubsequence(List(1,2),List(1,2,3)), "hasSubsequence doesn't work")

    assert(!hasSubsequence(List(1,2,4),List(1,2,3)), "hasSubsequence doesn't work")
  }
}
