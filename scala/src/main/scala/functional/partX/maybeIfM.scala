package functional.part4

import functional.part1.maybePart4._

object maybeIfM {

  def ifM[A](cond: Maybe[Boolean], ifTrue: => Maybe[A], ifFalse: => Maybe[A]): Maybe[A] = cond match {
    case Just(true) => ifTrue
    case _ => ifFalse
  }

}
