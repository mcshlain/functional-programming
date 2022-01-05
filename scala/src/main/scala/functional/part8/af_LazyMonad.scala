package functional.part8

import functional.part3.applicative.*
import functional.part3.monad.*

object af_LazyMonad {


  case class Lazy[A](eval: () => A)

  // For any S[_] we can define a Monad (it's just data we don't need to define how anything actually behaves)
  given Monad[Lazy] with {

    override def pure[A](a: A): Lazy[A] = Lazy(() => a)

    override def flatMap[A, B](ma: Lazy[A], f: A => Lazy[B]): Lazy[B] = {
      val r = ma.eval()
      f(r)
    }
  }


  def main(args: Array[String]): Unit = {

    val six = Monad[Lazy].pure(3).map(_ * 2)
    val five = Monad[Lazy].pure(5)

    val result = Monad[Lazy].map2(six, five, _ + _)

    print(result.eval())

  }



}