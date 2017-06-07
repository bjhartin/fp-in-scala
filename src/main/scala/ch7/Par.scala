package ch7

import scala.concurrent.duration.TimeUnit

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
    def timeElapsedInMillis(unit: TimeUnit): Long = 0
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // was get(a)
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // Exercise 7.1
  //  def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = ???

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def a[A,B,C](f: A => B, g: B => C): A => C = {
    g compose f // g(f(_))
  }

  /*

  Claim: map(map(y)(g))(f) == map(y)(f compose g)

  Assumption 1: A Par[A] can be deconstructed as unit(a)
  Law 1: map(unit(x))(f) == unit(f(x))

  Simplifying the left hand side:

  1. map(map(y)(g))(f) -->
       map(map(unit(z))(g))(f) [by assumption 1]
  2. map(map(unit(z))(g))(f) -->
       map(unit(g(z)))(f) [by law 1]
  3. map(unit(g(z)))(f) -->
       unit(f(g(z))) [by law 1]

  Simplifying the right hand side:

  1. map(y)(f compose g) -->
       map(unit(z))(f compose g) [by assumption 1]
  2. map(unit(z))(f compose g) -->
       unit((f compose g)(f)) [by law 1]
  3. unit((f compose g)(f)) -->
       unit(f(g(z))) [by def. of compose]
   */

//  def fork[A](a: => Par[A]): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })


  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // NOTE: The act of 'unwrapping' an A from a Par[A] causes it to block.
  // In this way, the context of the computation is deferral, and
  // map and similar functions let us compose operations without
  // losing the deferral.  This is just like how they work for any functor,
  // allowing us to compose computations without losing the context.

  /* Exercise 7.2

  Would it be sufficient to add `run()` to the trait such that it evaluated in
  a new thread?  If this were the case, what would `fork` do?

   */

  // Excercise 7.3 - Just the basic idea.
  def map2WithTimeouts[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
  (es: ExecutorService) => {
    import scala.math.max

    val af = a(es)
    val bf = b(es)

    new Future[C] {
        override def get(timeout: Long, unit: TimeUnit): C = {
          val a = af.get(timeout, unit)
          val remaining = max(0, timeout - af.timeElapsedInMillis(unit))
          val b = bf.get(remaining, unit)
          f(a, b)
        }

        override def get: C = ???

        override def cancel(evenIfRunning: Boolean): Boolean = ???
        override def isDone: Boolean = ???
        override def isCancelled: Boolean = ???
        override def timeElapsedInMillis(unit: TimeUnit): Long = ???
      }
    }

  // Exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = {a: A =>
    lazyUnit(f(a))
  }
}


// A service which can submit something which
// returns a Callable[A], but has not yet been invoked,
// and returns a Future[A].
//
// Why would submit not simply take an => A?  Is this just
// a holdover from Java (since we're borrowing ExecutorService)?
trait ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

// Could just be an => A.
trait Callable[A] { def call: A }

// A deferred computation which:
// - Returns an A
// - May not be done
//   - Check 'isDone'
// - May be cancelled if not done
// - Will not block the thread unless 'get' is called.
//   - 'get' may be given a timeout
//   - note there is no way to timeout until 'get' is called, i.e.
//     no way to timeout the work on the allocated thread.
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
  def timeElapsedInMillis(unit: TimeUnit): Long // For exercise 7.3
}
