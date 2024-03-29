package functional.part8

import functional.part3.applicative.*
import functional.part3.monad.*

object ab_freeMonad {

  // Similar to our IO definition, but we extract the exact effect of Suspend in S[_]
  sealed trait FreeMonad[S[_], A]

  // a pure computation the immediately returns an A
  case class Pure[S[_], A](a: A) extends FreeMonad[S, A]

  // A suspension to the computation where resume is a some instruction in our instruction set
  // it has some effect and yields a result
  case class Suspend[S[_], A](resume: S[A]) extends FreeMonad[S, A]

  // A composition of two computations, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[S[_], A, B](sub: FreeMonad[S, A], f: A => FreeMonad[S, B]) extends FreeMonad[S, B]


  // For any S[_] we can define a Monad (it's just data we don't need to define how anything actually behaves)
  given [S[_]]: Monad[[X] =>> FreeMonad[S, X]] with {

    override def pure[A](a: A): FreeMonad[S, A] = Pure(a)

    override def flatMap[A, B](ma: FreeMonad[S, A], f: A => FreeMonad[S, B]): FreeMonad[S, B] = FlatMap(ma, f)
  }

  // Lift any instruction from the syntax S[A] into the free computation
  // (transform it into a more general form where we have a monad definition and thus can compose programs
  // into bigger programs through sequencing operations)
  inline def liftFM[S[_], A](value: S[A]): FreeMonad[S, A] = Suspend(value)

}
