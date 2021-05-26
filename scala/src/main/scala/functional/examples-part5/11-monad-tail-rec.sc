
import functional.part3.applicative._
import functional.part3.functor._
import functional.part3.monad._
import functional.part5.parserCombinatorMonadTailRec.{Parser => P, _}

// We can use for-yield now as well


val r3 = (P.str("a") | P.str("b")).repeat(3)


r3.run(Location(""))
r3.run(Location("a"))
r3.run(Location("ab"))
r3.run(Location("aba"))
r3.run(Location("abab"))
r3.run(Location("ababa"))


val at3 = (P.str("a") | P.str("b")).atMost(3)

at3.run(Location(""))
at3.run(Location("a"))
at3.run(Location("ab"))
at3.run(Location("aba"))
at3.run(Location("abaa"))
at3.run(Location("ababa"))
