
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
    case Leaf(_) => 1
  }

  def maxLeaf(tree: Tree[Int]): Int = tree match {
    case Branch(t1, t2) => maxLeaf(t1) max maxLeaf(t2)
    case Leaf(i) => i
  }

  def depth(tree: Tree[Int], currentDepth: Int = 1): Int = tree match {
    case Branch(t1, t2) => depth(t1, currentDepth + 1) max depth(t2, currentDepth + 1)
    case Leaf(i) => currentDepth
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Branch(t1, t2) => Branch(map(t1, f), map(t2, f))
    case Leaf(i) => Leaf(f(i))
  }

  // No accumulator is needed as for List's folds.
  // The accumulation is instead a separate function for handling the results
  // from two branches of a node that have already been folded.
  def fold[A, B](tree: Tree[A], combine: (B, B) => B, f: (A) => B): B = tree match {
    case Branch(t1, t2) => combine(fold(t1, combine, f), fold(t2, combine, f))
    case Leaf(a) => f(a)
  }

  def size2[A](tree: Tree[A]): Int =
    fold(tree, (i1: Int, i2: Int) => 1 + i1 + i2, (a: A) => 1)

  def maxLeaf2(tree: Tree[Int]): Int =
    fold(tree, (i1: Int, i2: Int) => i1 max i2, (i: Int) => i)

  def depth2(tree: Tree[Int]): Int =
    fold(tree, (i1: Int, i2: Int) => (i1 max i2) + 1, (i: Int) => 1)

  def map2[A,B](tree: Tree[A], f: A => B): Tree[B] =
    fold(tree, (t1: Tree[B], t2: Tree[B]) => Branch(t1, t2), (a:A) => Leaf(f(a)))
}