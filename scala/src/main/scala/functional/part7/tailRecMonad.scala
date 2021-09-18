package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*

import scala.annotation.tailrec

object tailRecMonad {

  // IO Monad implemented as data
  sealed trait TailRec[A]

  // a pure computation the immediatly returns an A
  // For example: pure will be implemented using this data constructor
  case class Pure[A](a: A) extends TailRec[A]

  // A suspension to the computation where resume is a function that takes not arguments
  // but has some effect and yields a result
  // For example: reading information from the user will be implemented using this data constructor
  case class Suspend[A](resume: () => A) extends TailRec[A]

  // A composition of two steps, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]


  // The Monad definition becomes trivial, the operations are just the corresponding
  // data constructors
  given Monad[TailRec] with {
    override def pure[A](a: A): TailRec[A] = Pure(a)
    override def flatMap[A, B](ma: TailRec[A], f: A => TailRec[B]): TailRec[B] = FlatMap(ma, f)
  }

  // The tail recursive interpreter of the IO data constructors
  @tailrec
  def run[A](io: TailRec[A]): A =
    io match {
      case Pure(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Pure(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) =>
          run(y.flatMap(a => g(a).flatMap(f))) // just two applications of flatMap
      }
    }


  def main(args: Array[String]): Unit = {
     // We're composing so many functions that when we evaluate g, it will be too big for the stack
     val f = (x: Int) => x + 1
     val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
     g(42)

    // Using the TailRec Monad we can do the same computation and make it stack safe!!!
//    val f: Int => TailRec[Int] = (x) => Return(x + 1)
//    val g = List.fill(10000)(f).foldLeft(f) {
//      // Reminder: Kleisli arrow (>=>) is just a way to compose functions that return a monadic result
//      // f: A => M[B]
//      // g: B => M[C]
//      // h: A => M[C]
//      // h === f >=> g === (a: A) => f(a).flatMap(b)
//      (a, b) => ((x: Int) => Suspend(() => x)) >=> a >=> b
////        (a, b) => a >=> b
//
//      // Another way of writing this:
////       (a, b) => (x: Int) => Suspend(() => ()).flatMap(_ => a(x).flatMap(b))
////          (a, b) => (x: Int) => Suspend(() => ()) >> a(x).flatMap(b)
////           (a, b) => Suspend(() => ()) >> (a >=> b)
//
//      // Yet another way
////      val escapeStack = Suspend(() => ())
////      (a, b) => {
////        (x: Int) =>
////          for {
////            _ <- escapeStack
////            y <- a(x)
////            h <- b(y)
////          } yield h
////      }
//    }
//    println(g(42))
//    println(run(g(42))) // lets interpret the calculation out of stack
  }


}