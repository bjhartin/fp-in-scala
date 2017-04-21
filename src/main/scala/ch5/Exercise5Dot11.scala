package ch5

object Exercise5Dot11 extends App {
  import Stream._

  /*
    unfold separates the state tracking from the element (f: S => Option[(A, S)]),
    but here (and often I think) current element *is* the state by which the decision
    is made to continue and from which the next element is generated.
   */

  def finite(i: Int) = {
    if (i >= 8)
      None
    else {
      val next = i * 2
      Some(next, next)
    }
  }

  def infinite(i: Int) = Some(i * 2, i * 2)

  assert(unfold(1)(finite).take(5) == Stream(2, 4, 8))
  assert(unfold(1)(infinite).take(5) == Stream(2, 4, 8, 16, 32))
}
