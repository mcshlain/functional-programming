
import functional.part3.applicative._
import functional.part3.functor._
import functional.part3.monad._
import functional.part5.parserCombinatorMonad.{Parser => P, _}

// We can use for-yield now as well

val anbnParser = for (
  as <- P.str("a").zeroOrMoreTimes();
  bs <- P.str("b").repeat(as.length)
) yield {
  as.mkString + bs.mkString
}

anbnParser.run(Location("aaaab"))
anbnParser.run(Location("abbbbb"))

anbnParser.run(Location(""))
anbnParser.run(Location("ab"))
anbnParser.run(Location("aabb"))
anbnParser.run(Location("aaabbb"))
anbnParser.run(Location("aaaabbbb"))

