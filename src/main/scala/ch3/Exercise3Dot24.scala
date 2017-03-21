object Exercise3Dot24 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    // Not a formal enumeration of edge cases, more driven by intuition and sloppy late-night thinking.

    assert(hasSubsequence(List(1,2,3),Nil), "Nil should be a subsequence of any sequence")

    assert(hasSubsequence(List(1,2,3),List(1,2)), "A subsequence at the start should be recognized")

    assert(hasSubsequence(List(1,2,3),List(2,3)), "A subsequence at the end should be recognized")

    assert(hasSubsequence(List(1,2,3,4),List(2,3)), "A subsequence in the middle should be recognized")

    assert(hasSubsequence(List(1),List(1)), "A matching sequence should be seen as a subsequence")

    assert(!hasSubsequence(List(1),List(2)), "A sequence with no intersection should not be a subsequence")

    assert(!hasSubsequence(List(1,2),List(1,2,3)), "A superset should not be a subsequence")

    assert(!hasSubsequence(List(1,2,4),List(1,2,3)), "One missing member is enough to throw us off")

    assert(!hasSubsequence(Nil,List(1,2,3)), "A non-Nil subsequence is not a subsequence of Nil")
  }
}
