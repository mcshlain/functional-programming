package functional.part5

import functional.part3.applicative._
import functional.part3.functor._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object parserCombinatorApplicativeWithLazyOr {

  // represents a location in the parsed string, input is the full string, offset is where 
  // the current parsing head is
  case class Location(input: String, offset: Int = 0) {

    // conveniance: build a new location by advancing the current one
    def advance(by: Int): Location = this.copy(offset = this.offset + by)

    // useful getters for understanding the position in the string
    def getLine(): Int = input.slice(0, offset + 1).count(_ == '\n') + 1

    def getColumn(): Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lastLineStart => offset - lastLineStart
    }
  }


  // location will indicate where the error happened and some message about what went wrong
  case class ParserError(loc: Location, msg: String)

  // This is basically a specialized Either with nicer names and only a single generic param 
  sealed trait Result[+A] {
    // it's good to have an easy way to convert to an actual either
    def toEither: Either[ParserError, (A, Location)] = this match {
      case Failure(err) => Left(err)
      case Success(v, loc) => Right((v, loc))
    }
  }
  case class Success[+A](get: A, location: Location) extends Result[A]
  case class Failure[+A](get: ParserError) extends Result[Nothing]

  // NOTE: We can define a Monad for result (like Either) but we wont be using so we will do with Functor 
  given Functor[Result] with {
    def map[A, B](fa: Result[A], f: A => B): Result[B] = fa match {
      case err@Failure(_) => err
      case Success(v, offset) => Success(f(v), offset)
    }
  }

  // parser 
  case class Parser[+A](run: Location => Result[A]) {

    // Add ability to change the errors, not just the results (similar to eithers mapLeft)
    def mapError(f: ParserError => ParserError): Parser[A] = Parser {
      loc =>
        run(loc) match {
          case succ@Success(_,_) => succ
          case Failure(err) => Failure(f(err))
        }
    }

    def desc(name: String): Parser[A] = this.mapError(err =>
      ParserError(err.loc, f"Expected '$name' at ${err.loc.offset}" +
        f" but found: ${err.loc.input.slice(err.loc.offset, err.loc.offset + 10)}")
    )

    def or[B >: A] (pb: => Parser[B]): Parser[B] = Parser.or(this, pb)

    inline def |[B >: A] (pb: => Parser[B]): Parser[B] = Parser.or(this, pb)

    // derived from OR

    def optional(): Parser[Option[A]] =
      Applicative[Parser].map(this, Some(_)) | Applicative[Parser].pure(None)

    // derived from repeat and atMost
    def times(minimum: Int, maximum: Int): Parser[List[A]] =
      Applicative[Parser].map2(this.repeat(minimum), this.atMost(maximum - minimum), (p1, p2) => p1 ++ p2)

    // BETTER QUNTIFIERS, defined as basic parsers (a low level implmentation, not through combinators)
    def repeat(times: Int): Parser[List[A]] = Parser.repeat(this, times)
    def atMost(times: Int): Parser[List[A]] = Parser.atMost(this, times)

    def zeroOrMoreTimes(): Parser[List[A]] = times(0, Int.MaxValue)
    def oneOrMoreTimes(): Parser[List[A]] = times(1, Int.MaxValue)

    // another useful variation of quantifiers (with delimiters)
    def oneOrMoreTimes[B](delimiterParser: Parser[B]): Parser[List[A]] =
      // for example with x and a delimiter of ',', this is just saying: x followedBy (,x)*
      (this ** (delimiterParser >> this).zeroOrMoreTimes()).map((h, t) => h :: t)

    def zeroOrMoreTimes[B](delimiterParser: Parser[B]): Parser[List[A]] =
       oneOrMoreTimes(delimiterParser) | Applicative[Parser].pure(List.empty)

    // same versions but with string delimiter
    def oneOrMoreTimes(delimiter: String): Parser[List[A]] =
      this.oneOrMoreTimes(Parser.str(delimiter))

    def zeroOrMoreTimes(delimiter: String): Parser[List[A]] =
      this.zeroOrMoreTimes(Parser.str(delimiter))

  }

  // functor applicative for parser
  given Applicative[Parser] with {

    def pure[A](a: A): Parser[A] = Parser {
      loc => Success(a, loc) // pure values don't consume anything from the input
    }

    override def map2[A, B, C](pa: Parser[A], pb: Parser[B], f: (A, B) => C): Parser[C] = Parser {
      loc0 =>
        pa.run(loc0) match {
          case err@Failure(_) => err
          case Success(v1, loc1) =>
            pb.run(loc1) match {
              case err@Failure(_) => err
              case Success(v2, loc2) =>
                Success(f(v1, v2), loc2)
            }
        }
    }
  }

  // Product will be very useful so lets create an operator for it
  extension [A, B](pa: Parser[A]) {
   
    def **(pb: Parser[B]): Parser[(A, B)] = Applicative[Parser].product(pa, pb)

  }
  
  // --------------------------------- // 
  // Basic concrete parsers definition //
  // --------------------------------- // 

  object Parser {

    def str(expected: String): Parser[String] = Parser {
      loc =>
        if (loc.input.substring(loc.offset).startsWith(expected)) {
          Success(
            expected,
            loc.advance(expected.length)
          )
        } else {
          Failure(
            ParserError(
              loc,
              f"Expected '$expected' at ${loc.offset}"
                + f" but got '${loc.input.slice(loc.offset,loc.offset + expected.length)}'"
            )
          )
        }
    }

    def regex(expectedRegex: Regex): Parser[String] = Parser {
      loc =>
        expectedRegex.findPrefixOf(loc.input.substring(loc.offset)) match {
          case Some(m) =>
            Success(
              m.toString,
              loc.advance(m.length)
            )
          case None =>
            Failure(
              ParserError(
                loc,
                f"Expected '/${expectedRegex.pattern}/' at ${loc.offset}"
                  + f" but got '${loc.input.slice(loc.offset, loc.offset + 10)}'"
              )
            )
        }
    }

    def or[A] (pa: Parser[A], pb: => Parser[A]): Parser[A] = Parser {
      loc =>
        pa.run(loc) match {
          case succ@Success(_, _) => succ
          case Failure(_) => pb.run(loc)
        }
    }

    // BETTER QUANTIFIERS: We can't define them the way we want with combinators, lets just make them into basic
    // operations, and we will have it efficient by implementing it with a tail recursion
    def repeat[A](pa: Parser[A], times: Int): Parser[List[A]] = Parser(
      loc => {

        @tailrec
        def go(collected: List[A], location: Location, n: Int): Result[List[A]] = {
          if (n <= 0){
            Success(collected, location)
          } else {
            pa.run(location) match {
              case err@Failure(_) => err
              case Success(v, nextLocation) =>
                go(v :: collected, nextLocation, n - 1)
            }
          }
        }

        // we assembled the list in the opposite order for efficiency, now we reverse it to get the correct order
        Functor[Result].map(go(List.empty, loc, times), _.reverse)
      }
    )

    def atMost[A](pa: Parser[A], times: Int): Parser[List[A]] = Parser(
      loc => {

        @tailrec
        def go(collected: List[A], location: Location, n: Int): Result[List[A]] = {
          if (n <= 0){
            Success(collected, location)
          } else {
            pa.run(location) match {
              case err@Failure(_) =>
                Success(collected, location) // The only difference between this implementation and repeat
              case Success(v, nextLocation) =>
                go(v :: collected, nextLocation, n - 1)
            }
          }
        }

        // we assembled the list in the opposite order for efficiency, now we reverse it to get the correct order
        Functor[Result].map(go(List.empty, loc, times), _.reverse)
      }
    )

  }

}
