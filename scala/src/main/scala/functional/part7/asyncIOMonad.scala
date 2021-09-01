package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*
import functional.part6.al_TaskAsMonadFixed.*

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.io.StdIn

object asyncIOMonad {

  // IO Monad implemented as data
  sealed trait AsyncIO[A]

  // a pure computation the immediatly returns an A
  // For example: pure will be implemented using this data constructor
  case class Return[A](a: A) extends AsyncIO[A]

  // A suspension to the computation where resume is an async Task (this is the only change
  // to the data representation of IO)
  case class Suspend[A](resume: Task[A]) extends AsyncIO[A]

  // A composition of two steps, when the interpreter will encounter this it will first process
  // the 'sub' computation and then based on its result process the second computation (available through f)
  case class FlatMap[A, B](sub: AsyncIO[A], f: A => AsyncIO[B]) extends AsyncIO[B]


  // The Monad definition becomes trivial, the operations are just the corresponding
  // data constructors
  given Monad[AsyncIO] with {
    override def pure[A](a: A): AsyncIO[A] = Return(a)
    override def flatMap[A, B](ma: AsyncIO[A], f: A => AsyncIO[B]): AsyncIO[B] = FlatMap(ma, f)
  }

  // The tail recursive interpreter of the async IO data constructors
  @tailrec
  def step[A](async: AsyncIO[A]): AsyncIO[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def interpret[A](async: AsyncIO[A]): Task[A] = step(async) match {
    case Return(a) => Monad[Task].pure(a)
    case Suspend(r) => r.flatMap(a => interpret(Return(a)))
    case FlatMap(x, f) => x match {
      case Suspend(r) => r.flatMap(a => interpret(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  // basic IO Operations, now suspended inside a task
  def PrintLine(s: String): AsyncIO[Unit] =
    Suspend(Task{
      println(s)
    })

  val ReadLine: AsyncIO[String] = {
    Suspend(Task {
      StdIn.readLine()
    })
  }

  def forever[A](ma: AsyncIO[A]): AsyncIO[Nothing] = {
    lazy val t = forever(ma)
    ma.flatMap(_ => t)
  }

  def main(args: Array[String]): Unit = {

    // Examples of programs that use the combinators (identical to non async version)
    val foreverExample = forever {
      for {
        _ <- PrintLine("Input something")
        input <- ReadLine
        _ <- PrintLine(s"You entered: $input")
      } yield ()
    }

    // This interprets out program as a Task, it doesn't run it
    val mainTask = interpret(foreverExample)

    given ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

    mainTask.execute // now execute our program as a task (which will be multi threaded)
  }


}