package functional.part5

import functional.part3.applicative._
import functional.part3.functor._
import functional.part5.parserCombinatorApplicativeWithLazyOr._
import functional.part5.parserCombinatorApplicativeWithLazyOr.Parser._

object jsonParser {

  trait Json

  case class JsonObject(entries: Map[String, Json]) extends Json
  case class JsonArray(values: List[Json]) extends Json
  case class JsonNumber(value: Double) extends Json
  case class JsonString(value: String) extends Json
  case class JsonBool(value: Boolean) extends Json


  val booleanValue = (str("true") | str("false")).map(v => JsonBool(v == "true"))
  val numericValue = regex("-?\\d+(\\.\\d+)?".r).map(v => JsonNumber(v.toDouble))
  val stringValue = (str("\"") >> regex("[^\"]+".r) << str("\"")).map(v => JsonString(v))

  val primitiveValue = booleanValue | numericValue | stringValue

  primitiveValue.run(Location("true"))
  primitiveValue.run(Location("-13.7"))
  primitiveValue.run(Location("\"abc\""))


  // with a recursive definition the compiler needs the value to have a type definition
  val array: Parser[JsonArray] =
    (str("[") >> (primitiveValue | array | objectP).zeroOrMoreTimes(delimiter=",") << str("]")).map(JsonArray(_))

  array.run(Location("[]"))
  array.run(Location("[true]"))
  array.run(Location("[true,12.6,\"word\"]"))
  array.run(Location("[true,[12.6,14.2],\"word\"]"))



  // Here what we've seen with lazy parameters is not enough, because we need to define objectEntry in terms of object
  // and object in terms of ojectEntry ???
  // luckaly we can also define variables as lazy, here the entire right hand side of the assignment will be lazy


  lazy val objectKey = (str("\"") >> regex("[^\"]+".r) << str("\""))

  // again we need a type hint here, or the compiler wont be able to figure out the recursive structure
  lazy val objectEntry: Parser[(String, Json)] = objectKey ** (str(":") >> (primitiveValue | array | objectP))
  lazy val objectP: Parser[JsonObject] =
    (str("{") >> objectEntry.zeroOrMoreTimes(",") << str("}")).map(entries => JsonObject(entries.to(Map)))

  objectP.run(Location("{\"name\":\"john\",\"age\":29,\"address\":{\"street\":\"Hertzel\",\"no\":12}}"))

  // We still can't have arrays that contain objects or objects that contain arrays with the parser we defined
  // but we now have all the building blocks in place (homework?)
}
