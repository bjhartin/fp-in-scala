object Exercise3Dot27 extends App {
  override def main(args: Array[String]): Unit = {
    import Tree._


    assert(depth(Leaf(1)) == 1, "Depth doesn't work.")

    assert(depth(Branch(Leaf(1), Leaf(1))) == 2, "Depth doesn't work.")

    assert(depth(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 3, "Depth doesn't work.")
  }
}