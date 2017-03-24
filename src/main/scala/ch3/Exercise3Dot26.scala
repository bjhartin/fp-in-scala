object Exercise3Dot26 extends App {
  override def main(args: Array[String]): Unit = {
    import Tree._

    assert(maxLeaf(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4, "Max doesn't work.")
    assert(maxLeaf(Branch(Branch(Leaf(11), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 11, "Max doesn't work.")
  }
}