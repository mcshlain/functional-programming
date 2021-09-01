package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*
import functional.part7.hasTailRecM.*

import scala.io.StdIn

object ioMonadMoreCombinators {

  case class IO[+A](run: () => A) // suspended computatation that returns A

  // give the Monad definition to IO
  given Monad[IO] with {

    override def pure[A](a: A): IO[A] = IO(() => a)

    override def flatMap[A, B](ma: IO[A], f: A => IO[B]): IO[B] = IO {
      () =>
        val a = ma.run()
        f(a).run()
    }
  }

  // give a monoid definition to IO
  // we can only define a monoid for this IO if A itself forms a monoid
  given [A: Monoid]: Monoid[IO[A]] with {
    override def empty: IO[A] = IO{
      () => summon[Monoid[A]].empty
    }

    override def combine(ma: IO[A], mb: IO[A]): IO[A] = {
      Applicative[IO].map2(ma, mb, (a, b) => Monoid[A].combine(a, b))
    }
  }

  // lets define Unit as Monoid (trivial)
  given Monoid[Unit] with {
    override def empty: Unit = ()
    override def combine(x: Unit, y: Unit): Unit = ()
  }

  given HasTailRecM[IO] with {

    def tailRecM[A, B](init: A)(fn: A => IO[Either[A, B]]): IO[B] = IO {
      () => {
        var nextValue = init
        var finalResult: Option[B] = None

        while (finalResult.isEmpty) {
          val op = fn(nextValue)

          op.run() match {
            case Right(b) =>
              finalResult = Some(b)
            case Left(a) =>
              nextValue = a
          }
        }

        finalResult.get
      }
    }
  }

  // define some basic IO operations
  def PrintLine(msg: String): IO[Unit] = IO {
    () => println(msg)
  }

  val ReadLine: IO[String] = IO {
    () => StdIn.readLine()
  }

  // Additional combinators (All these are not IO specific and can work on different Monads)

  // notice that for a pure condition value, we can just use a regular if statement so this combinator
  // is only useful if the condition has some effect
  def ifM[M[_]: Monad, A](cond: M[Boolean], ifTrue: => M[A], ifFalse: => M[A]): M[A] =
    cond.flatMap(ok => if(ok) ifTrue else ifFalse)


  // do an effect in an endless loop
  def forever[M[_]:Monad: HasTailRecM, A](ma: M[A]): M[Nothing] = {
    HasTailRecM[M].tailRecM[Unit, Nothing](()) {
      _ => ma.map(_ => Left(()))
    }
  }


  // do an effect unitl the effectful condition not longer works
  def doWhile[M[_]:Monad: HasTailRecM, A](ma: M[A])(cond: A => M[Boolean]): M[Unit] = {
    HasTailRecM[M].tailRecM[Boolean, Unit](true) {
      keepGoing => {
        if (keepGoing) {
          ma.flatMap(cond).map(b => Left(b)) // based on the condition we returned whether we should stop the recursion
        } else {
          Monad[M].pure(Right(())) // returning a Right value indicates we want to stop
        }
      }
    }
  }


  // fold a lazy list with an effectful function, combining the effects
  // NOTE: intentionally not implemented with tailRecM to demonstrate a StackOverflow
  // Homework assignmet: implement with tailRecM
  def foldM[M[_]: Monad, A, B](lst: LazyList[A])(zero: B)(f: (B, A) => M[B]): M[B] = {
    lst match {
      case h #:: t => f(zero, h).flatMap(z2 => foldM(t)(z2)(f))
      case _ => Monad[M].pure(zero)
    }
  }


  // -------- //
  // Examples //
  // -------- //


  val ifMExample =
    PrintLine("Input some character") >>
    ifM(ReadLine.map(_ == "x"), PrintLine("Got x. :)"), PrintLine("Didn't get X. :("))

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

  val foldMExample = foldM[IO, Int, Int]((1 to 10).to(LazyList))(0){
    (acc, v) => PrintLine(s"Accamulator so far: $acc").map(_ => acc + v)
  }

  def main(args: Array[String]): Unit = {

    val comp = ifMExample
//    val comp = doWhileExample
//    val comp = foreverExample
//    val comp = foldMExample

    comp.run()
  }
}
