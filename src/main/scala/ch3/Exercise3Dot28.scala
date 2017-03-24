object Exercise3Dot28 extends App {
  override def main(args: Array[String]): Unit = {
    import Tree._

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f = (i:Int) => i.toString

    assert(map(tree, f) == Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4"))), "Map didn't work")
  }
}