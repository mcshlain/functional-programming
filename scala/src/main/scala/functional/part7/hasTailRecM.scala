package functional.part7

object hasTailRecM {

  // lets generalize the ability to define a tailRecM implementation
  trait HasTailRecM[M[_]] {
    def tailRecM[A, B](init: A)(fn: A => M[Either[A, B]]): M[B]
  }

  object HasTailRecM {
    // for better syntax
    def apply[M[_]: HasTailRecM]: HasTailRecM[M] = summon[HasTailRecM[M]]
  }

}
