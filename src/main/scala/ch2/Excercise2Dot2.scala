object Exercise2Dot2 extends App {
  override def main(args: Array[String]): Unit = {
    if(args.length != 0)
      println("USAGE: Exercise2Dot2")
    else {
      val sortedInts = Array(1,2,3,4)
      val compareInts = (a:Int, b:Int) => a <= b

      val unsortedStrings = Array("z", "b", "m")
      val compareStrings = (a: String, b: String) => a <= b // Pretend it's different for Strings :)

      println(s"${sortedInts.mkString(",")} sorted? : ${isSorted(sortedInts, compareInts)}")
      println(s"${unsortedStrings.mkString(",")} sorted? : ${isSorted(unsortedStrings, compareStrings)}")
    }
  }


  // Implementing this intentionally in the style from chapter 2, i.e. not taking advantage
  // of foldLeft, etc.
  @annotation.tailrec
  private[this] def isSorted[A](as: Array[A], ordered: (A,A) => Boolean, sortedSoFar: Boolean = true): Boolean = {
    val len = as.length
    val sorted = sortedSoFar && ordered(as(0), as(1))

    if(as.length > 2)
      isSorted(as.drop(1), ordered, sorted)
    else
      sorted
  }
}
