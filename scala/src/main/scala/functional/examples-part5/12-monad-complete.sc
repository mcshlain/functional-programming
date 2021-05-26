
import functional.part3.applicative._
import functional.part3.functor._
import functional.part3.monad._

import functional.part5.parserCombinatorMonadLooseEnds.{Parser => P}

// We changed the underlying abstraction to monad and everything works the same (as expected)

val anbnParser = for (
  as <- P.str("a").zeroOrMoreTimes();
  bs <- P.str("b").repeat(as.length)
) yield {
  as.mkString + bs.mkString
}

anbnParser.parse("aaaab")
anbnParser.parse("abbbbb")

anbnParser.parse("")
anbnParser.parse("ab")
anbnParser.parse("aabb")
anbnParser.parse("aaabbb")
anbnParser.parse("aaaabbbb")
