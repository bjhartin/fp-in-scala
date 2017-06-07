package ch6

import ch6.Chapter6._
import Random.{SimpleRNG, Rand}

object Chapter6Exercises extends App {
  import Random.RNG._

  def rng = SimpleRNG(1)

  val randDouble: Rand[Double] = rng => {
    val (a, rng2) = rng.nextInt
    (a.toDouble, rng2)
  }

  // 6.1
  assert(nonNegativeInt(rng)._1 == 384748)

  // 6.2
  assert(double(rng)._1 == 2.5991038289997608E-6)

  // 6.3
  assert(intDouble(rng)._1 == (384748, 2.5991038289997608E-6))
  assert(doubleInt(rng)._1 == (2.5991038289997608E-6, 384748))
  assert(double3(rng)._1 == (2.5991038289997608E-6, 8.686192992829177E-10, 1.8202209720228633E-9))

  // 6.4
  assert(ints(3)(rng)._1 == List(384748, 384748, 384748))

  // 6.5
  assert(double2(rng)._1 == 2.5991038289997608E-6)

  // 6.6
  def f(i: Int, d: Double) = s"The int is '$i' and the double is $d"
  val randString = map2(int, randDouble)(f)
  assert(randString(SimpleRNG(2))._1 == "The int is '769497' and the double is 1.988230381E9", "6.6 failed")

  // 6.7
  assert(ints2(5)(rng)._1 == List(-883454042, 1612966641, -549383847, -1151252339, 384748))

  // 6.8
  assert(nonNegativeLessThan(5)(rng)._1 == 3)

  //6.9
  assert(map(int)(_.toString)(rng) == mapViaFlatMap(int)(_.toString)(rng))
  assert(map2(int, randDouble)(f)(rng) == map2ViaFlatMap(int, randDouble)(f)(rng))

  //6.10
  val s1 = Stateful({i:Int => (i+1, i*i)})
  val s2 = Stateful({i:Int => (i+1, i/i)})

  assert(Stateful.sequence(List(s1, s2)).run(1) == (3, List(4, 1)))
}
