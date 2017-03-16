object Exercise3Dot7 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    /*

    Question: Can product, implemented using foldRight, immediately halt the recursion and
return 0.0 if it encounters a 0.0? Why or why not?

    Answer: No, because foldRight itself controls the recursion, and the short-circuiting behavior
    is specific to multiplication, i.e. it makes sense to abort 1 * 0 * 2 * 3, but not 1 + 0 + 2 + 3.
    The function we pass in does not influence the recursion.

    Question: Consider how any short-circuiting might work if you call foldRight with a large list.

    Answer: Not sure here...doesn't seem like we can short circuit.  However, if we could, we'd still
    be vulnerable to stack overflows since our foldRight isn't tail recursive.

     */
  }
}
