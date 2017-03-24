object Exercise3Dot25 extends App {
  override def main(args: Array[String]): Unit = {
    import Tree._

    assert(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7, "Size doesn't work.")
    assert(size(Branch(Leaf(3), Leaf(4))) == 3, "Size doesn't work.")
  }
}