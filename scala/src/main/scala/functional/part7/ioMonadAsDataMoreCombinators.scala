package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*

import scala.annotation.tailrec
import scala.io.StdIn

object ioMonadAsDataMoreCombinators {

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


  // More combinators
  // we made them specific to IO this time because only for IO we created this tail recursive interpreter
  // so for other monads these implementations are not stack safe

  def forever[A](ma: IO[A]): IO[Nothing] = {
    lazy val t = forever(ma)
    ma.flatMap(_ => t)
  }


  def doWhile[A](ma: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
    a <- ma
    keepGoing <- cond(a)
    _ <- if(keepGoing) doWhile(ma)(cond) else Monad[IO].pure(())
  } yield ()


  def foldM[A, B](lst: LazyList[A])(zero: B)(f: (B, A) => IO[B]): IO[B] = {
    lst match {
      case h #:: t => f(zero, h).flatMap(z2 => foldM(t)(z2)(f))
      case _ => Monad[IO].pure(zero)
    }
  }


  // Examples of programs that use the combinators
  val foreverExample = forever {
    for {
      _ <- PrintLine("Input something")
      input <- ReadLine
      _ <- PrintLine(s"You entered: $input")
    } yield ()
  }

  val doWhileExample = for {
    _ <- PrintLine("Enter something, q for exit")
    _ <- doWhile(ReadLine){ input =>
      if (input == "q") {
        PrintLine(s"Exiting based on user request").map(_ => false)
      } else {
        PrintLine(s"got $input").map(_ => true)
      }
    }
  } yield ()

  // Now this large fold will no longer crash!!!
  val foldMExample = foldM[Int, Int]((1 to 100000).to(LazyList))(0){
    (acc, v) => PrintLine(s"Accamulator so far: $acc").map(_ => acc + v)
  }


  def main(args: Array[String]): Unit = {
    val comp = doWhileExample
//  val comp = foreverExample
//  val comp = foldMExample

    run(comp)
  }

}