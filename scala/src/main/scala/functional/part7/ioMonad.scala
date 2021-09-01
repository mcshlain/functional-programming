package functional.part7

import functional.part3.applicative.*
import functional.part3.monad.*
import functional.part3.monoid.*

import scala.io.StdIn

object ioMonad {

  case class IO[A](run: () => A) // suspended computatation that returns A

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

  // define some basic IO operations
  def PrintLine(msg: String): IO[Unit] = IO {
    () => println(msg)
  }

  val ReadLine: IO[String] = IO {
    () => StdIn.readLine()
  }

  // working converter example
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0


  // fully pure converter, it just creates the description of our program
  // the for-yield structure makes it look like a regular effectful program
  val converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit:")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  // a version without for-yield (so we have other constructs, not justu the imperative style)
  val converter2: IO[Unit] = {
    PrintLine("Enter a temperature in degrees Fahrenheit:") >> ReadLine.flatMap{
      d =>
        PrintLine(fahrenheitToCelsius(d.toDouble).toString)
    }
  }

  // this will be our definition of our pureMain (this is a general form of how a main function will look like
  // in a pure setting with the IO monad)
  def pureMain(args: List[String]): IO[Unit] = {
    converter |+| converter2 // monoid combine (since Unit is also monoid)
  }

  // the actual main will just ask for the description of the full program from the pureMain
  // and then run the "interpreter" (only here will side effects occur)
  def main(args: Array[String]): Unit = {
    val fullDescription = pureMain(args.to(List))
    fullDescription.run() // interpreter
  }

}