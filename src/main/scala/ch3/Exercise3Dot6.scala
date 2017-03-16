object Exercise3Dot6 extends App {
  override def main(args: Array[String]): Unit = {
    import List._

    assert(init(List(1,2,3,4,5)) == List(1,2,3,4), "Lists weren't equal")
    assert(init(List(1)) == Nil, "Lists weren't equal")
    assert(init(Nil) == Nil, "Didn't work for Nil")

    // Question: Why can’t this function be implemented in constant time like
    // tail?

    // Answer: Like any singly linked list, we walk from head to tail and can't know we're at the
    // tail until we get there.  Thus we don't have a `Cons2(xs, x)` which accesses the last
    // element in constant time.
  }
}
