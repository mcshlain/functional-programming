package functional.part3

object monoid {

  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = summon[Monoid[A]]
  }

  given Monoid[Int] with {
    def empty = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  given Monoid[String] with {
    def empty = ""

    def combine(x: String, y: String): String = x.concat(y)
  }

  given [A]:Monoid[List[A]] with {
    def empty: List[A] = List.empty

    def combine(x: List[A], y: List[A]): List[A] = x.concat(y)
  }

  given [A]:Monoid[A => A] with {
    def empty: A => A = identity

    def combine(x: A => A, y: A => A): A => A = x andThen y
  }

  given [A: Monoid, B: Monoid]:Monoid[(A, B)] with {
    def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    def combine(x: (A, B), y: (A, B)): (A, B) = (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }

  given [K, V: Monoid]:Monoid[Map[K, V]] with {
    def empty: Map[K, V] = Map.empty

    def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
      val combinedMap = x.toList.concat(y.toList).groupMap { case (k, _) => k }(_._2)
      combinedMap.view.mapValues {
        case List(a) => a
        case List(a, b) => Monoid[V].combine(a, b)
        case _ => throw AssertionError("Ha?")
      }.toMap
    }
  }

  // To all monoids add |+| operator as combine
  extension[A: Monoid](x: A) {
    def |+|(y: A): A = Monoid[A].combine(x, y)
  }

  // Add  to lists if elements form a monoid
  extension[A: Monoid](lst: List[A]) {
    def combineAll = lst.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }

}
