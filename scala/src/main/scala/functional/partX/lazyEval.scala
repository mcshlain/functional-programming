package functional.part4

object lazyEval {

  case class Lazy[+A](eval: () => A) {
    // we need as methods for the for-yield construct to work
    def map[B](f: A => B): Lazy[B] = LazyEval.map(this, f)

    def flatMap[B](f: A => Lazy[B]): Lazy[B] = LazyEval.flatMap(this, f)
  }

  object LazyEval {
    def pure[A](v: A): Lazy[A] = Lazy(() => v)

    def map[A, B](ma: Lazy[A], f: A => B): Lazy[B] = Lazy(() => f(ma.eval()))

    def flatMap[A, B](ma: Lazy[A], f: A => Lazy[B]): Lazy[B] = Lazy {
      () => f(ma.eval()).eval()
    }

    def ap[A, B](ma: Lazy[A], ff: Lazy[A => B]): Lazy[B] = Lazy(() => {
      val a = ma.eval()
      val f = ff.eval()
      f(a)
    })

    // exactly the same implementation we had for Maybe (again using ap and map)
    def map2[A, B, C](a: Lazy[A], b: Lazy[B], f: (A, B) => C): Lazy[C] = ap(b, map(a, f.curried))

    def map3[A, B, C, D](a: Lazy[A], b: Lazy[B], c: Lazy[C], f: (A, B, C) => D): Lazy[D] = ap(c, ap(b, map(a, f.curried)))

    // Another useful function that is worth knowing
    def product[A, B](a: Lazy[A], b: Lazy[B]): Lazy[(A, B)] = map2(a, b, (_, _))
  }

}
