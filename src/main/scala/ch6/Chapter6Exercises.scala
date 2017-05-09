package ch6

import ch6.Random.{SimpleRNG, Rand, RNG}

object Chapter6Exercises extends App {
  import Random.RNG._

  // 6.1
  assert(nonNegativeInt(SimpleRNG(1))._1 == 384748)

  // 6.2
  assert(double(SimpleRNG(1))._1 == 2.5991038289997608E-6)

  // 6.3
  assert(intDouble(SimpleRNG(1))._1 == (384748,2.5991038289997608E-6))
  assert(doubleInt(SimpleRNG(1))._1 == (2.5991038289997608E-6,384748))
  assert(double3(SimpleRNG(1))._1 == (2.5991038289997608E-6,8.686192992829177E-10,1.8202209720228633E-9))

  // 6.4
  assert(ints(3)(SimpleRNG(1))._1 == List(384748, 384748, 384748))

  // 6.5
  assert(double2(SimpleRNG(1))._1 == 2.5991038289997608E-6)

  // 6.6
  val randDouble: Rand[Double] = rng => {
    val (a, rng2) = rng.nextInt
    (a.toDouble, rng2)
  }

  def f(i: Int, d: Double) = s"The int is '$i' and the double is $d"

  val randString = map2(RNG.int, randDouble)(f)

  assert(randString.apply(SimpleRNG(1))._1 == "The int is '384748' and the double is -1.151252339E9", "6.6 failed")
  assert(randString.apply(SimpleRNG(2))._1 == "The int is '769497' and the double is 1.988230381E9", "6.6 failed")

  // 6.7
  assert(ints2(5)(SimpleRNG(1))._1 == List(-883454042, 1612966641, -549383847, -1151252339, 384748))

  // 6.8
  assert(nonNegativeLessThan(5)(SimpleRNG(1))._1 == 3)
}
