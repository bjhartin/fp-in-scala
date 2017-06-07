package ch7

object Chapter7 {
  import Par._

  // In these examples I'll use some vars to help me detect when certain things happen.

  // 7.4
  var x = 0
  val f = {i:Int => x = x + 1; i + 1}
  val async = asyncF(f)

//  async(1)
//  println(x)
//

  object Exercise7 extends App {
    val f: String => Boolean = _.contains("a")
    val g: Int => String = _.toString()
    val y: Par[Int] = unit(1)

    // Claim: map(map(y)(g))(f) = map(y)(f compose g)

    val a1: Par[Boolean] = map(map(y)(g))(f)
    val a2: Par[Boolean] = map(y)(f compose g)
  }
}
