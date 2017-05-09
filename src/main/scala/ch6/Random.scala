package ch6

import ch6.Random.RNG

object Random {
  /*

  Something about the structure of these types seems wrong.

  Rand is parameterized but defines a function in terms of an RNG which only deals in Ints.

  That doesn't make sense to me.

   */

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    // At first glance, this appears like something's wrong because Rand is parameterized
    // and this method has 'int' in the name (and param type).
    //
    // It works because creating a random int is the basis for all other random values,
    // so any Rand[A] needs nextInt.
    def nextInt: (Int, RNG)
  }

  object RNG {
    val int: Rand[Int] = rng => rng.nextInt

    //  Once we have map, we can implement Rand for other types very concisely.
    //  We need only provide a function which converts a random int to the type at hand.
    //  val double: Rand[Double] = map(int)(_.toDouble)
    //  val string: Rand[String] = map(int)(_.toString)

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt
      val adjusted = if (i == Int.MinValue) i + 1 else i
      (math.abs(adjusted), rng2)
    }

    // 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (i, rng2) = nonNegativeInt(rng)
      val adjusted = if (i == 0) 1 else i
      (1.0 / adjusted, rng2)
    }

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double(rng)
      ((i, d), rng3)
    }

    // 6.3
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }

    // 6.3
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = double(rng)
      val (d2, rng3) = double(rng2)
      val (d3, rng4) = double(rng3)
      ((d1, d2, d3), rng4)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (scala.List[Int], RNG) = {
      if (count == 0)
        (Nil: scala.List[Int], rng)
      else {
        val (i, rng2) = rng.nextInt
        val (is, rng3) = ints(count - 1)(rng)
        (i :: is, rng3)
      }
    }

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    // 6.5
    def double2: Rand[Double] = map(nonNegativeInt)(i => 1.0 / (if (i == 0) 1 else i))

    // 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      fs.foldRight((Nil: List[A], rng)) {(rand, acc) =>
        val (a, rng2) = rand(acc._2)
        (a :: acc._1, rng2)
      }
    }

    def ints2(count: Int)(rng: RNG): (scala.List[Int], RNG) = {
      sequence(List.fill(count)(int))(rng)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) ({ i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          unit[Int](mod)
        else
          nonNegativeLessThan(n)
      })
    }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng2) = f(rng)
      // We have to apply the Rand[B] generated from g,
      // or else we'd be returning a Rand[Rand[B]], a.k.a. RNG => Rand[B],
      // instead of the required Rand[B]
      g(a)(rng2)
    }

    def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s) ({a =>
        unit(f(a))
      })
    }

//    NEXT
//    def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//
//    }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
    override def toString: String = s"SimpleRNG(${seed.toInt})"
  }

}