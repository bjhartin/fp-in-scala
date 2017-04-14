package ch5

object Exercise5Dot7 extends App {
  val x = Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Empty)))
  
  assert(x.map(_ * 2).toString == "Cons(4, Cons(6, Cons(8, Empty)))")
  assert(x.filter(_ % 2 == 0).toString == "Cons(2, Cons(4, Empty))")
  assert(x.append(Cons(() => 5, () => Empty)).toString == "Cons(2, Cons(3, Cons(4, Cons(5, Empty))))")
  assert(x.flatMap(a => Cons(() => a, () => Empty)).toString == "Cons(2, Cons(3, Cons(4, Empty)))")
}
