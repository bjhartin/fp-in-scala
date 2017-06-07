package ch7

object Exercise7Dot7 extends App {
  import Par._

  object Exercise7 extends App {
    // Claim: map(map(y)(g))(f) = map(y)(f compose g)

    // Law 1 (1st functor law): map(y)(id) == y
    // Law 2: map(unit(x))(f) == unit(f(x))

    /*

     The book says law 2 follows from law 1 because of parametricity.

     Map is defined as:

      def map[A,B](pa: Par[A])(f: A => B): Par[B]

     Because it's parametrized on A and B, then we know a lot about
     what it *can't* do.  It can't behave differently for different function types
     or Par types.

     So, once we know that map(y)(id) == y, we know that all map does is apply the
     function in a new Par.

     Why does this follow from the law map(y)(id) == y and not simply the signature?

     Is this akin to saying that, without that law, map(y)(id) might produce a par
     that was not equal to y?

     Would this still all hold if there were other kinds of Pars than that produced by unit()?

     Anyhow, from law2, the claim follows.  The claim, by the way, is the second functor law.

    */


    val f: String => Boolean = _.contains("a")
    val g: Int => String = _.toString()
    val y: Par[Int] = unit(1)

    val left: Par[Boolean] = map(map(y)(g))(f)
    val right: Par[Boolean] = map(y)(f compose g)

    val left1: Par[Boolean] = map(map(unit(1))(g))(f) // def of Par / unit
    val left2: Par[Boolean] = map(unit(g(1)))(f)      // by law 2
    val left3: Par[Boolean] = unit(f(g(1)))           // by law 2

    val right1: Par[Boolean] = map(unit(1))(f compose g) // def of Par / unit
    val right2: Par[Boolean] = unit((f compose g)(1))    // by law 2
    val right3: Par[Boolean] = unit(f(g(1)))             // by def. of compose
  }
}
