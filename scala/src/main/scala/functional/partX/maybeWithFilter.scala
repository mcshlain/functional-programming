package functional.part4

import functional.part1.maybePart4._

object maybeWithFilter {

  // scala 3 syntax to add methods to existing classes (in scala 2 this can be done with implicit classes conversions)
  extension [A](m: Maybe[A]) {
    def withFilter(f: A => Boolean): Maybe[A] = m match {
      case same@Just(v) => if (f(v)) same else None
      case None => None
    }
  }
  
}
