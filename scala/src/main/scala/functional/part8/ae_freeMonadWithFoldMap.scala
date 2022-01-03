package functional.part8

import functional.part8.ad_naturalTransformation.*
import functional.part3.applicative.*
import functional.part3.monad.*

object ae_freeMonadWithFoldMap {

  // Similar to our IO definition, but we extract the exact effect of Suspend in S[_]
  sealed trait FreeMonad[S[_], A]

  // a pure computation the immediatly returns an A
  // For example: pure will be implemented using this data constructor
  case class Pure[S[_], A](a: A) extends FreeMonad[S, A]

  // A suspension to the computation where resume is a function that takes not arguments
  // but has some effect and yields a result
  // For example: reading information from the user will be implemented using this data constructor
  case class Suspend[S[_], A](resume: S[A]) extends FreeMonad[S, A]

  // A composition of two steps, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[S[_], A, B](sub: FreeMonad[S, A], f: A => FreeMonad[S, B]) extends FreeMonad[S, B]


  // For any S[_] we can define a Monad (it's just data we don't need to define how anything actually behaves)
  given [S[_]]: Monad[[X] =>> FreeMonad[S, X]] with {

    override def pure[A](a: A): FreeMonad[S, A] = Pure(a)

    override def flatMap[A, B](ma: FreeMonad[S, A], f: A => FreeMonad[S, B]): FreeMonad[S, B] = FlatMap(ma, f)
  }

  // Lift any container S[A] into the more general free form
  inline def liftFM[S[_], A](value: S[A]): FreeMonad[S, A] = Suspend(value)


  // We need to a way to transform our free representation into some monad, so that it can actually be used
  // and not just remain a data structure
  // NOTE: This is a simple implementation but can also be converted to a stack safe version with tailRecursion
  //       With a very similar implementation to the one we saw in AsyncIO in part 7

  // NOTE: it's useful to think of "nt" as just a function but instead of being over types, say S -> M like a regular
  //       function it's a function that on type constructors S and M, so that given S[A] produces a
  //       M[A] for every A (for any A)
  def foldMap[M[_]: Monad, S[_], A](fm: FreeMonad[S, A], nt: S ~> M): M[A] =
    fm match {
      case Pure(a) => Monad[M].pure(a)     // we know M is a Monad so it has a pure function we can summon
      case Suspend(resume) => nt(resume)   // resume is of type S, just apply nt to produce M
      case FlatMap(sub, f) =>
        // we have a free sub computation we can transform into the monad M
        // that monad (we can foldMap, but our f return a free computation again, so we just transform that
        // also with foldMap)
        val foldedSub = foldMap(sub, nt)
        foldedSub.flatMap(r => foldMap(f(r), nt))
    }

}
