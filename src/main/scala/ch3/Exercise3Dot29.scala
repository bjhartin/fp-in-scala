object Exercise3Dot29 extends App {
  override def main(args: Array[String]): Unit = {
    import Tree._

    assert(size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7, "Size2 doesn't work.")

    assert(maxLeaf2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4, "Max2 doesn't work.")

    assert(depth2(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 3, "Depth2 doesn't work.")

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f = (i:Int) => i.toString

    assert(map2(tree, f) == Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4"))), "Map2 didn't work")

  }
}