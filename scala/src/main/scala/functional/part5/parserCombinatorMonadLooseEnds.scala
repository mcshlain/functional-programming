package functional.part5

import functional.part3.applicative._
import functional.part3.functor._
import functional.part3.monad._

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object parserCombinatorMonadLooseEnds {

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

    def parse(input: String): Either[ParserError, A] =
      (this << Parser.EOF).run(Location(input))
        .toEither
        .map(_._1) // only take the result, the offset doesn't matter (it's an internal implementation detail)

    // Add ability to change the errors, not just the results (similar to eithers mapLeft)
    def mapError(f: ParserError => ParserError): Parser[A] = Parser {
      loc =>
        run(loc) match {
          case succ@Success(_, _) => succ
          case Failure(err) => Failure(f(err))
        }
    }

    def desc(name: String): Parser[A] = this.mapError(err =>
      ParserError(err.loc, f"Expected '$name' at ${err.loc.offset}" +
        f" but found: ${err.loc.input.slice(err.loc.offset, err.loc.offset + 10)}")
    )

    def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] = Parser {
      loc => {
        var location = loc
        var acc: A = init
        var finalResult: Option[Result[B]] = None

        while (finalResult.isEmpty) {
          val result = fn(acc).run(location)

          result match {
            case err@Failure(_) =>
              finalResult = Some(err)
            case Success(Right(b), l) =>
              finalResult = Some(Success(b, l))
            case Success(Left(a), l) =>
              location = l
              acc = a
          }
        }

        finalResult.get
      }
    }

    def or[B >: A](pb: Parser[B]): Parser[B] = Parser(
      loc =>
        this.run(loc) match {
          case succ@Success(_, _) => succ
          case Failure(_) => pb.run(loc)
        }
    )

    inline def |[B >: A](pb: Parser[B]): Parser[B] = or(pb)

    // derived from OR

    def optional(): Parser[Option[A]] =
      Applicative[Parser].map(this, Some(_)) | Applicative[Parser].pure(None)

    // derived from repeat and atMost
    def times(minimum: Int, maximum: Int): Parser[List[A]] =
      Applicative[Parser].map2(this.repeat(minimum), this.atMost(maximum - minimum), (p1, p2) => p1 ++ p2)

    // BETTER QUNTIFIERS, defined as basic parsers (a low level implmentation, not through combinators)
    def repeat(times: Int): Parser[List[A]] = Parser.repeat(this, times)

    def atMost(times: Int): Parser[List[A]] = Parser.atMost(this, times)

    // these definition are fine but because of atMost implemnetation this will actually
    // crash the stack :(
    def zeroOrMoreTimes(): Parser[List[A]] = times(0, Int.MaxValue)

    def oneOrMoreTimes(): Parser[List[A]] = times(1, Int.MaxValue)
  }

  // functor applicative for parser
  given Monad[Parser] with {

    def pure[A](a: A): Parser[A] = Parser {
      loc => Success(a, loc) // pure values don't consume anything from the input
    }

    def flatMap[A, B](pa: Parser[A], f: A => Parser[B]): Parser[B] = Parser {
      loc0 =>
        pa.run(loc0) match {
          case err@Failure(_) => err
          case Success(v1, loc1) =>
            f(v1).run(loc1) match {
              case err@Failure(_) => err
              case Success(v2, loc2) =>
                Success(v2, loc2)
            }
        }
    }
  }

  // Product will be very useful so lets create an operator for it
  extension[A, B] (pa: Parser[A]) {

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
                + f" but got '${loc.input.slice(loc.offset, loc.offset + expected.length)}'"
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

    // BETTER QUANTIFIERS: We can't define them the way we want with combinators, lets just make them into basic
    // operations, and we will have it efficient by implementing it with a tail recursion
    def repeat[A](pa: Parser[A], times: Int): Parser[List[A]] =
      Monad[Parser].map(
        pa.tailRecM[List[A], List[A]](List.empty)(
          lst =>
            if (lst.length == times) {
              Monad[Parser].pure(Right(lst))
            } else {
              Monad[Parser].map(pa, r => Left(r :: lst))
            }
        ), _.reverse)


    def atMost[A](pa: Parser[A], times: Int): Parser[List[A]] =
      Monad[Parser].map(
        pa.tailRecM[List[A], List[A]](List.empty)(
          lst =>
            if (lst.length == times) {
              Monad[Parser].pure(Right(lst))
            } else {
              Monad[Parser].map(pa, r => Left(r :: lst)) | Monad[Parser].pure(Right(lst))
            }
        ), _.reverse)


    val EOF: Parser[Unit] = Parser(
      loc => {
        if (loc.offset == loc.input.length) {
          Success((), loc)
        } else {
          Failure(
            ParserError(
              loc,
              f"Expected 'EOF' at ${loc.offset}"
                + f" but got '${loc.input.slice(loc.offset, loc.offset + 10)}'"
            )
          )
        }
      }
    )

  }

}
