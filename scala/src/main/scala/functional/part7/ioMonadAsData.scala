package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*

import scala.annotation.tailrec
import scala.io.StdIn

object ioMonadAsData {

  // IO Monad implemented as data
  sealed trait IO[A]

  // a pure computation the immediatly returns an A
  // For example: pure will be implemented using this data constructor
  case class Pure[A](a: A) extends IO[A]

  // A suspension to the computation where resume is a function that takes not arguments
  // but has some effect and yields a result
  // For example: reading information from the user will be implemented using this data constructor
  case class Suspend[A](resume: () => A) extends IO[A]

  // A composition of two steps, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[A, B](sub: IO[A], f: A => IO[B]) extends IO[B]




  // The Monad definition becomes trivial, the operations are just the corresponding
  // data constructors
  given Monad[IO] with {
    override def pure[A](a: A): IO[A] = Pure(a)
    override def flatMap[A, B](ma: IO[A], f: A => IO[B]): IO[B] = FlatMap(ma, f)
  }


  // The tail recursive interpreter of the IO data constructors
  @tailrec
  def run[A](io: IO[A]): A =
    io match {
      case Pure(a) => a
      case Suspend(r) => r()

      // The following implementation of the FlatMap case will not be tail recursive so we match on yet another level
      // to make the entire run/interpreter tail recursive
      //
      //  case FlatMap(x, f) => {
      //    val a = run(x)
      //    run(f(a))
      //  }

      // By matching again on x we eliminate the need to call run() twice, making the function tail recursive
      case FlatMap(x, f) => x match {
        case Pure(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) =>
          run(y.flatMap(a => g(a).flatMap(f))) // just two applications of flatMap
      }
    }


  // basic operations (effects are always encloused in a Suspend)
  def PrintLine(s: String): IO[Unit] =
    Suspend(() => println(s))

  val ReadLine: IO[String] =
    Suspend(() => StdIn.readLine())


  def main(args: Array[String]): Unit = {
    val program = for {
      _ <- PrintLine("Enter some text")
      txt <- ReadLine
      _ <- PrintLine(s"The User entered: '$txt'")
    } yield ()

    run(program)
  }

}