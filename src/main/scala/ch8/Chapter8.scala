package ch8

object Exercise1 {
  /*
    Properties for summing a list of ints:

    - Summing an empty list should be zero.
    - Summing a list should give the same value as summing the reversed list.
    - Summing a list of length N, where each value is M, should give N*M.
   */
}

object Exercise2 {
  /*
    Properties for finding the maximum int in a list:

    - The max of an empty list should be None.
    - The max of any other list should be Some(max).
      - All other elements of the list should be less than or equal to max.
   */
}

object Exercise3 extends App {
  trait Prop {
    def check: Boolean = ???
    def &&(p: Prop): Boolean = p.check && check
  }
}

object Exercise4 extends App {
//  case class Gen[A](sample: State[RNG,A])

  trait Prop {
    def check: Boolean = ???
    def &&(p: Prop): Boolean = p.check && check
  }
}